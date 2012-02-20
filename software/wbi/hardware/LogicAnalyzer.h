#ifndef LOGICANALYZER_H
#define LOGICANALYZER_H

#include <QMetaType>

#include <vector>
#include <exception>
#include <math.h>

#include "WBI.h"
#include "DataTrace.h"

/** @brief General Logic Analyzer Exception */
class LogicAnalyzerException : public std::exception {
    virtual const char* what() const throw() {return "General LogicAnalyzerException";}
};

/** @brief Connection Exception */
class LogicAnalyzerConnectionException : public LogicAnalyzerException {
    virtual const char* what() const throw() {return "LogicAnalyzerConnectionException";}
};

/** @brief Unsupported Hardware Exception */
class LogicAnalyzerNoSupportException : public LogicAnalyzerException {
    virtual const char* what() const throw() {return "LogicAnalyzerNoSupportException";}
};

enum LogicAnalyzerMuxType {
    MUX, ISR
};

struct LogicAnalyzerConfiguration {
 // Version
    int interfaceVersion;
    int minorHardwareVersion;

 // Mux
    /** Number of banks connected to the input multiplexer */
    int bankCount;

    /** Width of each input bank */
    int bankWidth;

    /** Type of input multiplexer */
    LogicAnalyzerMuxType muxType;

 // Trigger
    /** Number of trigger input bits */
    int triggerInputWidth;

    /** Number of edge triggers */
    int triggerEdgeCount;

    /** Number of value triggers */
    int triggerValueCount;

 // Memory
    /** Width of a sampling memory's address */
    int memoryAddressWidth;

    /** Width of a sampling memory's data */
    int memoryDataWidth;

    /** Max Address of sampling memory -> size */
    int memoryMaxAddress;

 // Misc
    /** True if client has a test pattern generator */
    bool supportTestPattern;

    /** True if clients supports run length encoding */
    bool supportRLE;

    /** True if clients supports reading of all registers */
    bool supportNonEssentialReads;

    /** True if clients supports asynch. reads of data inputs */
    bool supportAsynchronousReads;

    /** Width of the clock divider (0 if not supported by client) */
    int clockDividerWidth;

    /** Returns a human-readable representation of the configuration struct */
    QString toString() const;
};

Q_DECLARE_METATYPE(LogicAnalyzerConfiguration)

struct LogicAnalyzerAddresses {
    /** number of registers that hold the input mux configuration */
    int sizeMuxSetup;

    /** number of registers that hold the clock divider configuration */
    int sizeClkThreshold;

    /** number of registers that hold one memory address */
    int sizeMemAddress;

    /** number of registers that hold the current input
     (0 if not supported by client) */
    int sizeCurData;

    /** number of registers that hold the trigger enable mask */
    int sizeTriggerEnableMask;

    /** number of registers that hold one trigger configuration vector
     (trigger-input width bits) */
    int sizeTriggerRegister;

    /** address of the status register */
    int status;

    /** first address of the input multiplexer configuration */
    int muxSetup;

    /** first address of the clock divider threshold */
    int clkThreshold;

    /** first address of the register that holds the max. memory address,
      i.e. the size of the client's sample memory */
    int memMaxAddress;

    /** first address of the register that holds the base offset,
      i.e. the number of pre-triggered samples */
    int memBaseOffset;

    /** first address of the register that holds the memory's base address. This
      memory address is an absolute memory address and is mapped into the wishbone
      address scheme be adding MemBase */
    int memBaseAddress;

    /** first address of the register that holds the memory's address of the sample
      written when trigger fired. This memory address is an absolute memory
      address and is mapped into the wishbone address scheme be adding
     MemBase */
    int memTrgAddress;

    /** first address of the register that holds the memory's address of the last
      sample written. This memory address is an absolute memory address and is
      mapped into the wishbone address scheme be adding MemBase */
    int memCurAddress;

    /** first address of the register that holds the current asynchronous data */
    int curData;

    /** first address of the trigger enable mask */
    int trgEnable;

    /** first address of the first edge trigger's register */
    int trgEdgeSetup;

    /** first address of the first value trigger's register  */
    int trgValueSetup;

    /** first address of sampling memory mapped into the wishbone address scheme*/
    int memBase;
};

enum LogicAnalyzerSamplingStatus {
    EMPTY, CAPTURING, FULL
};

enum LogicAnalyzerTriggerMasks {
    EDGE_EDGE, EDGE_SENSITIVITY,
    VALUE_VALUE, VALUE_SENSITIVITY
};

class LogicAnalyzer {
public:        
    explicit LogicAnalyzer(WBI *wbi);
    virtual ~LogicAnalyzer();

    /** Connects to client and constructs local data structures */
    void connect();

    /** Disconnects from client and deletes all local data structures.
      Reconnecting is possible */
    void disconnect();

    /** Return clients configuration
      @attention A connection is required */
    LogicAnalyzerConfiguration configuration() const;

// STATUS REGISTER RELATED
    /** Sends a reset signal to the client.

      @attention A connection is required

      @attention Hint: If a soft reset is issued the trigger may instantanously
        fire again, which can be misinterpreted as a failure of the reset
        command.

      @param coldReset If true client resets most (depends on hardware) runtime
        settings. If false only a reset on the sampling pipeline is issued, i.e.
        client gets ready for another sampling. */
    void resetDevice(bool coldReset = false);

    /** Reads sampling status
      @attention A connection is required */
    LogicAnalyzerSamplingStatus readSamplingStatus() const;

    /** Reads test pattern setup
      @attention A connection is required */
    int readTestPatternSetup() const;

    /** Write test pattern setup.
      @attention A connection is required */
    void writeTestPatternSetup(int pattern);

    /** Reads run length encoding enable bit
      @attention A connection is required */
    bool readRLEEnable() const;

    /** Write run length encoding enable bit
      @attention A connection is required */
    void writeRLEEnable(bool enable);

// INPUT SELECTION
    /** Reads multiplexer setup
      @attention A connection is required */
    BitVector readMuxSetup() const;

    /** Write multiplexer setup
      @attention A connection is required
      @param mask New Setup. If vector's size is smaller than the setup
        width, missing bits are filled with 0. If vector is bigger, they
        have to be 0 otherwise an assertion is issued */
    void writeMuxSetup(BitVector mask);

// CLOCK DIVIDER
    /** Reads clock divider threshold
      @attention Client has to support non-essential reads
      @attention A connection is required */
    BitVector readClockDividerThreshold() const;

    /** Write clock divider threshold
      @attention A connection is required
      @param mask New Setup. If vector's size is smaller than the setup
        width, missing bits are filled with 0. If vector is bigger, they
        have to be 0 otherwise an assertion is issued   */
    void writeClockDividerThreshold(BitVector const & mask);

// DATA
    /** Read asynchronous data
      @attention A connection is required
      @return BitVector with data input width (bankWidth*bankCount) */
    BitVector readAsynchronousData() const;

// TRIGGER
    /** Read trigger enable mask
      @attention Client has to support non-essential reads
      @attention A connection is required */
    BitVector readTriggerEnableMask(bool edge, bool value) const;

    /** Write trigger enable mask
      @attention A connection is required
      @param mask Lower Bits edge, more signifcant: Value Trigger. If vector
        is smaller than the setup width, missing bits are filled with 0. If
        vector is bigger, they have to be 0 otherwise an assertion is issued */
    void writeTriggerEnableMask(BitVector const & mask);

    /** Write trigger enable mask
      @attention A connection is required
      @param edge Enable mask of edge triggers. If vector
        is smaller than the setup width, missing bits are filled with 0. If
        vector is bigger, they have to be 0 otherwise an assertion is issued
      @param vale Enable mask of value triggers. If vector
        is smaller than the setup width, missing bits are filled with 0. If
        vector is bigger, they have to be 0 otherwise an assertion is issued */
    void writeTriggerEnableMask(BitVector const & edge,
                                BitVector const & value);


    /** Read Trigger Setup
      @attention A connection is required
      @param triggerId The index of the trigger in its class, i.e. there might
        exists multiple triggers with id 0 (namely an edge and an value trigger)
      @param maskType Decides which property (and which trigger) to read from */
    BitVector readTriggerSetup(unsigned int triggerId,
                              LogicAnalyzerTriggerMasks maskType) const;

    /** Read Trigger Setup
      @attention A connection is required
      @param triggerId The index of the trigger in its class, i.e. there might
        exists multiple triggers with id 0 (namely an edge and an value trigger)
      @param maskType Decides which property (and which trigger) to read from
      @param mask New Setup. If vector's size is smaller than the setup
        width, missing bits are filled with 0. If vector is bigger, they
        have to be 0 otherwise an assertion is issued */
    void writeTriggerSetup(unsigned int triggerId,
                           LogicAnalyzerTriggerMasks maskType,
                           BitVector const & mask);

// Memory Access
    /** Reads memory address of the 0-th entry in the ring buffer
      @attention Memory status has to be "CAPTURING" or "FULL"
      @attention A connection is required */
    int readMemoryBaseAddress() const;

    /** Reads memory address the last element written
      @attention A connection is required */
    int readMemoryCurrentAddress() const;

    /** Reads memory address of the entry that was written after the trigger
      initially fired.
      @attention A connection is required */
    int readMemoryTriggerAddress() const;

    /** Reads the number of entries in front of the data point that was written
      with the initial trigger event. This value is used to switch between pre-
      and post-triggering.
      @attention A connection is required */
    int readMemoryBaseOffset() const;

    /** Writes the number of entries in front of the data point that was written
      with the initial trigger event. This value is used to switch between pre-
      and post-triggering.
      @param offset Value from the interval [0; maxMemoryAddress] */
    void writeMemoryBaseOffset(int offset);

    /** Download raw data from client's sampling ring puffer.
      @attention Memory status has to be "CAPTURING" or "FULL"
      @attention A connection is required
      @param begin First index to download from
      @param end Last index exclusively. If -1 all valid data is loaded
      @return Vector of BitVectors. The i-th BitVector correspondes to the
        (i+begin) entry in the ring buffer */
    std::vector<BitVector> downloadRawData(int begin=0, int end=-1) const;

    /** Decode downloaded data assuming it was sampled with the client's current
      settings.
      @param data Downloaded raw data
      @return Vector where each entry represents one bank */
    std::vector<DataTrace> decodeData(std::vector<BitVector> const & data) const;

    /** Download and decode data from client.
      @attention Memory status has to be "CAPTURING" or "FULL"
      @attention A connection is required
      @return Vector where each entry represents one bank */
    std::vector<DataTrace> decodeData() const;

protected:
    WBI *_wbi;

    /** true if connected to client */
    bool _connected;

    /** Configuration read upon connecting */
    LogicAnalyzerConfiguration _config;

    /** Addresses computed from configuration */
    LogicAnalyzerAddresses _addresses;

    /** Read configuration from client */
    LogicAnalyzerConfiguration _readConfig() const;

    /** Calculate address scheme from configuration (read before)
      @param config Configuration address scheme is based on */
    LogicAnalyzerAddresses _calcAddresses(
        LogicAnalyzerConfiguration const & config) const;

    /** Resize vector and ensure that more significant bits are 0
      @param vec Source vector
      @param size New size */
    BitVector _formatVector(BitVector const & vec, int size) const;

    /** Calculates the address of the register requested */
    int _calcTriggerAddress(unsigned int triggerId,
                            LogicAnalyzerTriggerMasks maskType) const;
};


#endif // LOGICANALYZER_H
