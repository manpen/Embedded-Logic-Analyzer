#include "WBISerial.h"
#include <QTextStream>
#include <math.h>

#include <QTime>

WBISerial::WBISerial(QextSerialPort* port)
{
    _port = port;
    _retries = 2;
    disconnect();
}

WBISerial::~WBISerial() {
}

bool WBISerial::connect() {
    Q_ASSERT_X(_port->isOpen(), "connect", "Serial Port Object is not opened");
    Q_ASSERT_X(_port->queryMode() == QextSerialPort::Polling, "connect", "Serial Port Object has to be in polling query mode");
    disconnect();

    try {
        _connected = true;
        /* oh, that's pretty dirty, but as the rs232 protocol provides no way
         * to enumarate the busses width, we assume, we're connected to a logic analyzer
         * which holds the data width at address 2 */
        _wbBytesPerAddress = 1;
        _wbDataWidth = read(2).toInt();
        _wbBytesPerAddress = (int) ceil(_wbDataWidth / 8.0);
    } catch(...) {
        disconnect();
        return false;
    }

    return true;
}

void WBISerial::disconnect() {
    _wbDataWidth = 8;
    _wbBytesPerAddress = 1;

    _currentAddress = -1;
    _currentLength = -1;
    _connected = false;
}

void WBISerial::configurePort(QextSerialPort &port) {
    port.setBaudRate(BAUD115200);
    port.setFlowControl(FLOW_OFF);
    port.setParity(PAR_NONE);
    port.setDataBits(DATA_8);
    port.setStopBits(STOP_1);
    port.setTimeout(5);
}

void WBISerial::setRetries(int value) {
    Q_ASSERT(value >= 0);
    _retries = value;
}

BitVector WBISerial::read(unsigned int address)
    throw (WBITimeoutException, WBIInvalidResponseException) {
    Q_ASSERT(_connected);

    for(int i = _retries; i>=0; i--) {
        // empty input buffer
        if (_port->bytesAvailable())
            _port->readAll();

        _set_address(address, i != _retries);

        // communitcation with client (yup, only two lines ^^)
        #ifdef WBI_SERIAL_VERBOSE
            qDebug() << "WBIsend: r";
        #endif
        _port->write("r");
        QString response = _recv_result(_wbBytesPerAddress * 2 + 2);

        if (response.size() > 1 && response.endsWith("+")) {
            // seems no too bad
            if (response.size() - 2 > (int) _wbBytesPerAddress * 2)
                qWarning() << "read more bytes than expected";


            bool conversionSuccessful;

            BitVector result = BitVector::fromString(
                        response.left(_wbBytesPerAddress*2), 16,
                        _wbDataWidth, &conversionSuccessful);

            if (!conversionSuccessful) {
                WBI_WARNING("received non hex data");
                continue;
            }

            const int recv_crc =
                QString(response[response.size() - 2]).toInt(&conversionSuccessful, 16);

            if (!conversionSuccessful) {
                WBI_WARNING("received non hex char as crc-sum");
                continue;
            }

            if (result.crc(CRC_POLY, CRC_POLY_DEGREE) != recv_crc) {
                WBI_WARNING("non matching crc");
                continue;
            }

            _currentAddress++;  // after a successful read the client increments the address

            return result;
        } else if (i == 1 and response.isEmpty()) {
            throw WBITimeoutException();
        }
    }

    throw WBIInvalidResponseException();

    // unreachable ;)
}

std::vector<BitVector> WBISerial::readBlock(unsigned int address, unsigned int length)
    throw (WBITimeoutException, WBIInvalidResponseException) {
    Q_ASSERT(_connected);

    if(length==1) {
        // the read command is optimized for reading a single register,
        // thus let's use that (might save the _set_length call)

        return std::vector<BitVector>(1, read(address));
    }

    for(int i = _retries; i>=0; i--) {
        // empty input buffer
        if (_port->bytesAvailable())
            _port->readAll();

        _set_address(address, i != _retries);
        _set_length(length, i != _retries);

        #ifdef WBI_SERIAL_VERBOSE
            qDebug() << "WBIsend: M";
        #endif
        _port->write("M");

        QString response = _recv_result();

        if (response.size() - 2 == int(_wbBytesPerAddress * length * 2)
                && response.endsWith("+")) {
        // prepare container to store result
            std::vector<BitVector> result;
            result.reserve(length);

        // helper
            const int nibblesPerRegister = (int) ceil(_wbDataWidth / 4.0);
            int crc = 0;
            bool conversionSuccessful = true;

        // interpret response
            for(unsigned int i=0; i < length && conversionSuccessful; i++) {
                BitVector vec = BitVector::fromString(
                    response.mid(i*nibblesPerRegister, nibblesPerRegister), 16,
                    _wbDataWidth, &conversionSuccessful);

                result.push_back(vec);
                crc = vec.crc(CRC_POLY, 4, crc, true);
            }

            if (!conversionSuccessful) {
                WBI_WARNING("received non hex data");
                continue;
            }

            BitVector ref_crc = BitVector::fromString(
                response.mid(response.size()-2, 1), 16,
                4, & conversionSuccessful);

            if (!conversionSuccessful || ref_crc.crc(CRC_POLY, CRC_POLY_DEGREE, crc)) {
                WBI_WARNING("crc error");
                continue;
            }

            _currentAddress+=length;  // after a successful read the client increments the address

            return result;
        } else if (i == 1 and response.isEmpty()) {
            throw WBITimeoutException();
        }
    }

    throw WBIInvalidResponseException();

    // again - it's pretty unlikely to reach this point
}

void WBISerial::write(const BitVector & data, unsigned int address)
    throw (WBITimeoutException, WBIInvalidResponseException) {
    Q_ASSERT(_connected);
    Q_ASSERT(_wbBytesPerAddress > 0);

    QString sendBuffer;
    sendBuffer.reserve(3 + _wbBytesPerAddress*2);   //2 nibbles per byte + "Y" + crc + ";"
    QTextStream stream(&sendBuffer);
    stream.setIntegerBase(16);

    // build command
    stream << "Y"
           << (data == false ? "0" : data.toString(16, false));

    stream << data.slice(0, qMin(data.size(), sendBuffer.size()*4-4)).crc(
                  CRC_POLY, CRC_POLY_DEGREE, (int)CRC_CMD_CODE_WRITE, true)
           << ";";


    for (int i = _retries; i>=0; i--) {
        _set_address(address, i != _retries);
        #ifdef WBI_SERIAL_VERBOSE
            qDebug() << "WBIsend:" << sendBuffer;
        #endif
        _port->write(sendBuffer.toAscii());

        QString result = _recv_result(2);
        if (result.endsWith("+")) {
            // seems that everything is alright
            _currentAddress += 1;
            return;

        } else if (i == 1 and result.isEmpty()) {
            throw WBITimeoutException();
        }
    }

    throw WBIInvalidResponseException();
}

void WBISerial::writeBlock(const std::vector<BitVector> & data,
                           unsigned int address)
    throw (WBITimeoutException, WBIInvalidResponseException) {

    for(unsigned int i=0; i < data.size(); i++) {
        write(data[i], address+i);
    }
}

QString WBISerial::_recv_result(int const expectedLength) {
    char buf[1024];
    QString result;
    if (expectedLength)
        result.reserve(expectedLength);

    while (!result.endsWith("+") && !result.endsWith("-")) {
        QTime startTime = QTime::currentTime();

        while(_port->bytesAvailable() <= 0 &&
              startTime.msecsTo(QTime::currentTime()) < 1000);

        qint64 bytesRead = 0;
        if(_port->bytesAvailable() > 0)
            bytesRead = _port->read(buf, sizeof(buf));

        if (!bytesRead) {
            // timeout
            WBI_WARNING("timeout occured");
            return QString("");
        }

        result.append(QString::fromAscii(buf, bytesRead));
    }

    if (result.endsWith("-")) {
        WBI_WARNING("received failure indicator >-<");
        qDebug() << result;
    }

    result.squeeze();

#ifdef WBI_SERIAL_VERBOSE
    qDebug() << "WBIrecv:" << result;
#endif

    return result;
}

void WBISerial::_set_address(unsigned int address, bool force) {
    QString sendBuffer;
    QTextStream stream(&sendBuffer);

    //if (!force && _currentAddress == int(address))
    //    return;

    stream.setIntegerBase(16);
    stream << "X" << address;
    stream << BitVector::fromInt(address, sendBuffer.size()*4-4).crc(
                  CRC_POLY,CRC_POLY_DEGREE, (int) CRC_CMD_CODE_ADDRESS);
    stream << ";";

    for (int i = _retries; i>=0; i--) {
        #ifdef WBI_SERIAL_VERBOSE
            qDebug() << "WBIsend:" << sendBuffer;
        #endif

        _port->write(sendBuffer.toAscii());

        QString response = _recv_result();
        if (response.endsWith("+")) {
            _currentAddress = address;
            return;
        } else if (i == 1 and response.isEmpty()) {
            throw WBITimeoutException();
        }
    }

     throw WBIInvalidResponseException();
}

void WBISerial::_set_length(unsigned int length, bool force) {
    QString sendBuffer;
    QTextStream stream(&sendBuffer);

    /*if (!force
        && (_currentLength >= int(length))
        && (_currentLength - 2 <= int(length))
       )
        return;
*/
    stream.setIntegerBase(16);
    stream << "Z" << length;
    stream << BitVector::fromInt(length, sendBuffer.size()*4-4).crc(
                  CRC_POLY,CRC_POLY_DEGREE, (int) CRC_CMD_CODE_LENGTH);
    stream << ";";

    for (int i = _retries; i>=0; i--) {
        #ifdef WBI_SERIAL_VERBOSE
            qDebug() << "WBIsend:" << sendBuffer;
        #endif
        _port->write(sendBuffer.toAscii());

        QString response = _recv_result(2);
        if (response.endsWith("+")) {
            _currentLength = length;
            return;
        } else if (i == 1 and response.isEmpty()) {
            throw WBITimeoutException();
        }
    }

    throw WBIInvalidResponseException();
}
