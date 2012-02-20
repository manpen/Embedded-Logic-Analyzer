#include "LogicAnalyzer.h"
#include <QDebug>
#include <math.h>
#include <string>

QString LogicAnalyzerConfiguration::toString() const {
    QString result;
    QTextStream stream(&result);

    stream << "Interface version: "
           << interfaceVersion << "." << minorHardwareVersion << endl << endl;

    stream << "LogicAnalyzerMuxType: ";
    switch(muxType) {
    case MUX: stream << "MUX"; break;
    case ISR: stream << "ISR"; break;
    default: stream << "?"; break;
    }

    stream
        << endl
        << "bankCount: " << bankCount << endl
        << "bankWidth: " << bankWidth << endl
        << endl
        << "triggerInputWidth: " << triggerInputWidth << endl
        << "triggerEdgeCount: " << triggerEdgeCount << endl
        << "triggerValueCount: " << triggerValueCount << endl
        << endl
        << "memoryAddressWidth: " << memoryAddressWidth << endl
        << "memoryDataWidth: " << memoryDataWidth << endl
        << "memoryMaxAddress: " << memoryMaxAddress << endl
        << endl
        << "clockDividerWidth: " << clockDividerWidth << endl
        << "supportTestPattern: " << (supportTestPattern ? "Yes" : "No") << endl
        << "supportRLE: " << (supportRLE ? "Yes" : "No") << endl
        << "supportNonEssentialReads: " << (supportNonEssentialReads ? "Yes" : "No") << endl
        << "supportAsynchronousReads: " << (supportAsynchronousReads  ? "Yes" : "No") << endl
    ;

    return result;
}

//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//!! LOGIC ANALYZER
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#define REGISTER_NUMBER(x) int(ceil(float(x)/_wbi->getDataWidth()))
#define ADDRESS_WIDTH(x) int(ceil(log2(x)))
#define READ_REGISTERS(addr, regs) BitVector::fromVector(_wbi->readBlock(addr, regs))

#define ASSERT_CONNECTION if(!_connected) throw LogicAnalyzerConnectionException();

LogicAnalyzer::LogicAnalyzer(WBI *wbi)
{
  // initialize all components with NULL (todo: is that necessary ??)
    _wbi = wbi;
    disconnect();
}

LogicAnalyzer::~LogicAnalyzer() {
    disconnect();
}

///////////////////////////////////////////////////////////////////////////////
// CONNECTION HANDLING AND ENUMARATION
///////////////////////////////////////////////////////////////////////////////
void LogicAnalyzer::connect() {
    Q_ASSERT(!_connected);

    _config = _readConfig();
    Q_ASSERT((unsigned int)_config.memoryAddressWidth < 8*sizeof(int));

    _addresses = _calcAddresses(_config);

    // complete configuration
    _config.memoryMaxAddress = READ_REGISTERS(_addresses.memMaxAddress,
                                              _addresses.sizeMemAddress).toInt();

    _connected = true;
}

LogicAnalyzerConfiguration LogicAnalyzer::configuration() const {
    ASSERT_CONNECTION
    return _config;
}


LogicAnalyzerConfiguration LogicAnalyzer::_readConfig() const {
    LogicAnalyzerConfiguration config;

  // read complete base register set
    std::vector<BitVector> memoryBase = _wbi->readBlock(0, 0x10);
    BitVector & capRegister = memoryBase.at(0x0B);

  // extract info
    config.interfaceVersion = memoryBase[0x0].toInt();
    config.minorHardwareVersion = memoryBase[0x1].toInt();

    config.bankWidth = memoryBase[0x3].toInt();
    config.bankCount = memoryBase[0x4].toInt();

    config.memoryAddressWidth = memoryBase[0x5].toInt();
    config.memoryDataWidth    = memoryBase[0x6].toInt();
    config.memoryMaxAddress   = -1; // need to compute address scheme first !

    config.clockDividerWidth = memoryBase[0x7].toInt();

    config.triggerInputWidth = memoryBase[0x8].toInt();
    config.triggerEdgeCount  = memoryBase[0x9].toInt();
    config.triggerValueCount = memoryBase[0xA].toInt();

    config.supportAsynchronousReads = capRegister[3];
    config.supportNonEssentialReads = capRegister[2];
    config.supportRLE = capRegister[6];
    config.supportTestPattern = capRegister[4];

    int muxType = capRegister.slice(0,2).toInt();
    Q_ASSERT(muxType == 0 || muxType == 1);
    config.muxType = muxType ? ISR : MUX;

    return config;
}

LogicAnalyzerAddresses LogicAnalyzer::_calcAddresses(
    LogicAnalyzerConfiguration const & config) const {
    LogicAnalyzerAddresses addr;

// SIZE
    addr.sizeMuxSetup = REGISTER_NUMBER(config.muxType==ISR ?
                                            config.bankCount :
                                            ADDRESS_WIDTH(config.bankCount));

    addr.sizeClkThreshold = REGISTER_NUMBER(config.clockDividerWidth);
    addr.sizeMemAddress = REGISTER_NUMBER(config.memoryAddressWidth);
    addr.sizeCurData = REGISTER_NUMBER(config.bankCount * config.bankWidth);
    addr.sizeTriggerEnableMask = REGISTER_NUMBER(config.triggerEdgeCount +
                                                 config.triggerValueCount);
    addr.sizeTriggerRegister = REGISTER_NUMBER(config.triggerInputWidth);

 // ADDRESSES
    addr.status       = 0x10;
    addr.muxSetup     = addr.status   + 1;
    addr.clkThreshold = addr.muxSetup + addr.sizeMuxSetup;

    addr.memMaxAddress  = addr.clkThreshold   + addr.sizeClkThreshold;
    addr.memBaseOffset  = addr.memMaxAddress  + addr.sizeMemAddress;
    addr.memBaseAddress = addr.memBaseOffset  + addr.sizeMemAddress;
    addr.memTrgAddress  = addr.memBaseAddress + addr.sizeMemAddress;
    addr.memCurAddress  = addr.memTrgAddress  + addr.sizeMemAddress;

    addr.curData       = addr.memCurAddress + addr.sizeMemAddress;

    addr.trgEnable     = addr.curData      + addr.sizeCurData;
    addr.trgEdgeSetup  = addr.trgEnable    + addr.sizeTriggerEnableMask;
    addr.trgValueSetup = addr.trgEdgeSetup
            + 2 * config.triggerEdgeCount * addr.sizeTriggerRegister;

    addr.memBase       = 1 << config.memoryAddressWidth;

    return addr;
}

void LogicAnalyzer::disconnect() {
    _connected = false;
}

///////////////////////////////////////////////////////////////////////////////
// STATUS REGISTER RELATED (reset, test-pattern, encoding)
///////////////////////////////////////////////////////////////////////////////
void LogicAnalyzer::resetDevice(bool coldReset) {
    ASSERT_CONNECTION

    if (!coldReset) {
        /* Soft Reset -> Keep current configuration,
         so read status register first, set reset bit, and write it back */
        BitVector statusRegister;
        statusRegister = _wbi->read(0x10);
        statusRegister.setBit(6);   // soft reset
        _wbi->write(statusRegister, 0x10);
    } else {
        _wbi->write(BitVector::fromInt(0x80, 8), 0x10);

        /* reconnect in order to flush buffers */
        disconnect();
        connect();
    }
}

LogicAnalyzerSamplingStatus LogicAnalyzer::readSamplingStatus() const {
    ASSERT_CONNECTION
    int status = _wbi->read(_addresses.status).slice(0,2).toInt();

    Q_ASSERT(status);

    if (1 == status) return EMPTY;
    if (2 == status) return CAPTURING;
    return FULL;    // 3 == status, all other possibility covered by assert
}

int LogicAnalyzer::readTestPatternSetup() const {
    ASSERT_CONNECTION
    return _wbi->read(_addresses.status).slice(4,6).toInt();
}

void LogicAnalyzer::writeTestPatternSetup(int pattern) {
    ASSERT_CONNECTION
    Q_ASSERT(0 <= pattern && 3 >= pattern);

    BitVector tmp = _wbi->read(_addresses.status);
    tmp.fill(BitVector::fromInt(pattern, 2), 4);
    _wbi->write(tmp, _addresses.status);
}

bool LogicAnalyzer::readRLEEnable() const {
    ASSERT_CONNECTION
    return _wbi->read(_addresses.status)[3];
}

void LogicAnalyzer::writeRLEEnable(bool enable) {
    ASSERT_CONNECTION

    BitVector tmp = _wbi->read(_addresses.status);
    if (tmp[3] == enable) {
        // no change -> dont need to write
        return;
    }
    tmp.setBit(3, enable);
    _wbi->write(tmp, _addresses.status);
}

///////////////////////////////////////////////////////////////////////////////
// ASYNCHRONOUS DATA ACCESS
///////////////////////////////////////////////////////////////////////////////
BitVector LogicAnalyzer::readAsynchronousData() const {
    ASSERT_CONNECTION
    Q_ASSERT(_config.supportAsynchronousReads);

    BitVector data = READ_REGISTERS(_addresses.curData, _addresses.sizeCurData);
    data.resize(_config.bankCount * _config.bankWidth);
    return data;
}
///////////////////////////////////////////////////////////////////////////////
// CONNECTION HANDLING AND ENUMARATION
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// INPUT MULTIPLEXER
///////////////////////////////////////////////////////////////////////////////
BitVector LogicAnalyzer::readMuxSetup() const {
    ASSERT_CONNECTION
    if (_config.bankCount == 1) return BitVector(0);

    BitVector data = READ_REGISTERS(_addresses.muxSetup, _addresses.sizeCurData);
    data.resize(_config.muxType==ISR ?
                    _config.bankCount :
                    ADDRESS_WIDTH(_config.bankCount));
    return data;
}

void LogicAnalyzer::writeMuxSetup(BitVector mask) {
    ASSERT_CONNECTION
    Q_ASSERT(_config.bankCount > 1);

    int regSize = _config.muxType==ISR ?
        _config.bankCount :
        ADDRESS_WIDTH(_config.bankCount);

    if (mask.size() <= regSize) {
        Q_ASSERT(mask.slice(regSize) == false);
    }

    mask.resize(regSize);

    Q_ASSERT(_config.muxType != ISR || !(mask == false));

    _wbi->writeBlock(mask.split(_wbi->getDataWidth()), _addresses.muxSetup);
}

///////////////////////////////////////////////////////////////////////////////
// CLOCK DIVIDER
///////////////////////////////////////////////////////////////////////////////
BitVector LogicAnalyzer::readClockDividerThreshold() const {
    ASSERT_CONNECTION
    if (_config.clockDividerWidth == 0) return BitVector(0);
    if (!_config.supportNonEssentialReads) throw LogicAnalyzerNoSupportException();

    BitVector data = READ_REGISTERS(_addresses.clkThreshold,
                                    _addresses.sizeClkThreshold);
    data.resize(_config.clockDividerWidth);

    return data;
}

void LogicAnalyzer::writeClockDividerThreshold(BitVector const & mask) {
    ASSERT_CONNECTION
    if (!_config.clockDividerWidth) throw LogicAnalyzerNoSupportException();

    _wbi->writeBlock(
        _formatVector(mask, _config.clockDividerWidth).split(_wbi->getDataWidth()),
        _addresses.clkThreshold);
}

///////////////////////////////////////////////////////////////////////////////
// TRIGGER
///////////////////////////////////////////////////////////////////////////////
BitVector LogicAnalyzer::readTriggerEnableMask(bool edge, bool value) const {
    ASSERT_CONNECTION
    Q_ASSERT(edge || value);

    int enableMaskWidth = _config.triggerEdgeCount + _config.triggerValueCount;
    if (enableMaskWidth == 0) return BitVector(0);

    if (edge  && !value && !_config.triggerEdgeCount)  return BitVector(0);
    if (!edge && value  && !_config.triggerValueCount) return BitVector(0);

        if (!_config.supportNonEssentialReads) throw LogicAnalyzerNoSupportException();

    BitVector data = READ_REGISTERS(_addresses.trgEnable,
                                    _addresses.sizeTriggerEnableMask);

    if (edge && value) {
        return data.slice(0, enableMaskWidth);
    } else if (edge) {
        return data.slice(0, _config.triggerEdgeCount);
    } else {
        return data.slice(_config.triggerEdgeCount, _config.triggerValueCount);
    }


    return data;
}

void LogicAnalyzer::writeTriggerEnableMask(BitVector const & mask) {
    ASSERT_CONNECTION
    int enableMaskWidth = _config.triggerEdgeCount + _config.triggerValueCount;

    if (!enableMaskWidth) throw LogicAnalyzerNoSupportException();

    _wbi->writeBlock(
        _formatVector(mask, enableMaskWidth).split(_wbi->getDataWidth()),
        _addresses.trgEnable);
}

void LogicAnalyzer::writeTriggerEnableMask(BitVector const & edge,
                            BitVector const & value) {

    BitVector res(_config.triggerEdgeCount + _config.triggerValueCount);

    if (_config.triggerEdgeCount)
        res.fill(_formatVector(edge, _config.triggerEdgeCount), 0);

    if (_config.triggerValueCount)
        res.fill(_formatVector(value, _config.triggerValueCount),
                 _config.triggerEdgeCount);

    writeTriggerEnableMask(res);
}

BitVector LogicAnalyzer::readTriggerSetup(unsigned int triggerId,
                                     LogicAnalyzerTriggerMasks maskType) const {
    ASSERT_CONNECTION
    if (!_config.supportNonEssentialReads) throw LogicAnalyzerNoSupportException();

    int _addr = _calcTriggerAddress(triggerId, maskType);

    BitVector data = READ_REGISTERS(_addr,
                                    _addresses.sizeTriggerRegister);
    data.resize(_config.triggerInputWidth);

    return data;
}

void LogicAnalyzer::writeTriggerSetup(unsigned int triggerId,
                                      LogicAnalyzerTriggerMasks maskType,
                                      BitVector const & mask) {
    ASSERT_CONNECTION

    int _addr = _calcTriggerAddress(triggerId, maskType);

    _wbi->writeBlock(
        _formatVector(mask, _config.triggerInputWidth).split(_wbi->getDataWidth()),
        _addr);
}

int LogicAnalyzer::_calcTriggerAddress(unsigned int triggerId,
                                       LogicAnalyzerTriggerMasks maskType)const{
    switch(maskType) {
        case EDGE_EDGE:
        case EDGE_SENSITIVITY:
            Q_ASSERT(triggerId < (unsigned int)_config.triggerEdgeCount);
            return _addresses.trgEdgeSetup
                    + (2*triggerId + (maskType == EDGE_EDGE ? 0 : 1))
                        *_addresses.sizeTriggerRegister;

        case VALUE_VALUE:
        case VALUE_SENSITIVITY:
            Q_ASSERT(triggerId < (unsigned int)_config.triggerEdgeCount);
            return _addresses.trgValueSetup
                    + (2*triggerId + (maskType == VALUE_VALUE ? 0 : 1))
                        *_addresses.sizeTriggerRegister;
    }

    Q_ASSERT(false);
    return -1;
}

///////////////////////////////////////////////////////////////////////////////
// DOWNLOAD
///////////////////////////////////////////////////////////////////////////////
int LogicAnalyzer::readMemoryBaseAddress() const {
    ASSERT_CONNECTION
    return READ_REGISTERS(_addresses.memBaseAddress,
                          _addresses.sizeMemAddress).toInt();
}

int LogicAnalyzer::readMemoryCurrentAddress() const {
    ASSERT_CONNECTION
    return READ_REGISTERS(_addresses.memCurAddress,
                         _addresses.sizeMemAddress).toInt();
}

int LogicAnalyzer::readMemoryTriggerAddress() const {
    ASSERT_CONNECTION
    return READ_REGISTERS(_addresses.memTrgAddress,
                         _addresses.sizeMemAddress).toInt();
}

int LogicAnalyzer::readMemoryBaseOffset() const {
    ASSERT_CONNECTION
    if (!_config.supportNonEssentialReads) throw LogicAnalyzerNoSupportException();

    return READ_REGISTERS(_addresses.memBaseOffset,
                         _addresses.sizeMemAddress).toInt();
}

void LogicAnalyzer::writeMemoryBaseOffset(int offset) {
    Q_ASSERT(0 <= offset && offset < _config.memoryMaxAddress);

    ASSERT_CONNECTION

    _wbi->writeBlock(
        BitVector::fromInt(offset, _config.memoryAddressWidth).split(_wbi->getDataWidth()),
        _addresses.memBaseOffset);
}

std::vector<BitVector> LogicAnalyzer::downloadRawData(int begin, int end) const {
    ASSERT_CONNECTION
    LogicAnalyzerSamplingStatus samplingStatus = readSamplingStatus();

    if (samplingStatus == EMPTY) throw LogicAnalyzerException();

    Q_ASSERT(begin >= 0);
    if (end < 0) end += _config.memoryMaxAddress + 1;
    Q_ASSERT(end > begin && end <= _config.memoryMaxAddress);

 // read memory base address
    int baseAddress = readMemoryBaseAddress();

    int addressesValid = _config.memoryMaxAddress + 1;
    if (samplingStatus == CAPTURING) {
        int currentAddress = readMemoryCurrentAddress();
        addressesValid =
                (currentAddress-baseAddress + _config.memoryMaxAddress+1)
                    % (_config.memoryMaxAddress+1);
    }

    const int size = qMin(end - begin, addressesValid);

 // compute ring buffer indicies
    const int startAddress = (baseAddress + begin) % (_config.memoryMaxAddress+1);
    const int endAddress = (startAddress + size) % (_config.memoryMaxAddress+1);

    int firstChunkSize;
    if (startAddress >= endAddress) {
        // ring buffer rewarped while capturing, so the start address is actually
        // bigger than the end address. as the wbi supports only sequential block
        // reads, we have to break down the process into two seperate reads
        firstChunkSize = _config.memoryMaxAddress + 1 - startAddress;

    } else {
        // that's easy: read everthing in one go
        firstChunkSize = size;

    }

 // wb<->memory factor (if memoryDataWidth is bigger than the wb bus data width,
 // one memory address is broken down into the smallest power-of-2 unit possible
    int wbMemFactor = int(pow(2, ceil(
           log2(_config.memoryDataWidth / double(_wbi->getDataWidth())))));


 // download data
    std::vector<BitVector> rawData;
    rawData = _wbi->readBlock(_addresses.memBase + startAddress * wbMemFactor,
                              firstChunkSize * wbMemFactor);

    if (firstChunkSize != size) {
        std::vector<BitVector> secondChunk =
                _wbi->readBlock(_addresses.memBase,
                                (size-firstChunkSize) * wbMemFactor);

        rawData.insert(rawData.end(), secondChunk.begin(), secondChunk.end());
    }

// repack if necessary
    if (wbMemFactor == 1) return rawData;

    std::vector<BitVector> result;
    for(int i = 0; i < size; i++) {
        result.push_back(BitVector::fromVector(std::vector<BitVector>(
                                                &rawData.at(i*wbMemFactor),
                                                &rawData.at((i+1)*wbMemFactor))));
    }

    return result;
}

std::vector<DataTrace> LogicAnalyzer::decodeData() const {
    return decodeData(downloadRawData());
}

std::vector<DataTrace> LogicAnalyzer::decodeData(std::vector<BitVector> const & data) const {
    if (readRLEEnable()) {
        qWarning() << "LogicAnalyzer driver does not yet support RLE";
        return std::vector<DataTrace>();
    }

    std::vector<DataTrace> banks(_config.bankCount);

  // get active banks
    std::vector<int> activeBanks;
    if (_config.muxType == MUX) {
        activeBanks.push_back(readMuxSetup().toInt());
    } else {
        BitVector mux = readMuxSetup();
        for(int i=0; i < _config.bankCount; i++)
            if (mux[i])
                activeBanks.push_back(i);
    }

  // distribute data to banks
    unsigned int offset = (readMemoryTriggerAddress() - readMemoryBaseAddress()
                  + _config.memoryMaxAddress + 1) % activeBanks.size();

    for(unsigned int i = 0; i < offset; i++)
        banks.at(activeBanks[i]).push_back(DataFrame());

    for(unsigned int i = 0; i < data.size(); i++) {
        banks.at(activeBanks[(i+offset) % activeBanks.size()]).push_back(
                    DataFrame(data[i], 1));
    }

    if (offset) {
        for(unsigned int i = offset; i < activeBanks.size(); i++)
            banks.at(activeBanks[i]).push_back(DataFrame());
    }

    return banks;
}


///////////////////////////////////////////////////////////////////////////////
// COMMON TOOLS
///////////////////////////////////////////////////////////////////////////////
BitVector LogicAnalyzer::_formatVector(const BitVector &vec, int size) const {
    if (vec.size() == size)
        return BitVector(vec);

    BitVector tmp(size);

    if (vec.size() < size) {
        tmp.fill(false, vec.size());
    } else {
        Q_ASSERT(vec.slice(size) == false);
    }

    tmp.fill(vec);

    return tmp;
}
