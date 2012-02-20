/**
 @file
 @author Manuel Penschuck <manuel@penschuck.eu>
*/
/*
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef WBI_SERIAL_H_
#define WBI_SERIAL_H_

#define WBI_SERIAL_VERBOSE true

#include "WBI.h"
#include "../qextserial/qextserialport.h"

#include <QString>

/** Implementation of the Wishbone <-> RS232 protocoll */
class WBISerial : public WBI {
public:
    /**
      Constructor of WBISerial.

      @param port instance of a RS232 port. The port can still be closed, as no
                  communication is done until \ref connect() is called.
    */
    explicit WBISerial(QextSerialPort* port);
    ~WBISerial();

    /**
      Connected to the interface and reads the wishbone settings from client.
      @attention \ref _port is required to be configured and opened
    */
    bool connect();
    void disconnect();

    inline int getDataWidth() const {Q_ASSERT(_connected); return _wbDataWidth;}
    inline int getAddressWidth() const {return -1;}
    inline int getRetries() const {return _retries;}

    void setRetries(int value);

    BitVector read(unsigned int address)
        throw (WBITimeoutException, WBIInvalidResponseException);

    std::vector<BitVector> readBlock(unsigned int address, unsigned int length)
        throw (WBITimeoutException, WBIInvalidResponseException);

    void write(const BitVector & data, unsigned int address)
        throw (WBITimeoutException, WBIInvalidResponseException);

    void writeBlock(const std::vector<BitVector> & data, unsigned int address)
        throw (WBITimeoutException, WBIInvalidResponseException);

    /** Apply default configuration to serial port */
    static void configurePort(QextSerialPort& port);

protected:
    /** serial port instance that is used to communicate */
    QextSerialPort* _port;

    /** true when the instance is ready to communicate
      (after successfully calling @ref connect()) */
    bool _connected;

    /** Width of clients wishbone data line */
    unsigned int _wbDataWidth;
    /** ceil(\ref _wbDataWidth / 8.0) */
    unsigned int _wbBytesPerAddress;

    /** address cache that is used to avoid unnecessary address commands */
    unsigned int _currentAddress;
    /** length cache that is used to avoid unnecessary length commands */
    unsigned int _currentLength;

    /** number of retries if a command failes */
    int _retries;

// CRC
    /** crc start values used when calculating a commands crc */
    enum CRC_INITIAL_VALUE {
        CRC_CMD_CODE_ADDRESS = 11, // crc(0b0001),
        CRC_CMD_CODE_WRITE =    8, // crc(0b1101),
        CRC_CMD_CODE_LENGTH = 12 // crc(0b0110)
    };

    /** crc polynom used by client hardware (5bit polynom needed) */
    const static char CRC_POLY = 0b11011;
    /** degree of the crc generator polynome (BitVector supports upto 31) */
    const static int CRC_POLY_DEGREE = 4;

// IO
    /*!
     * Reads the client's response to a previously sent command.
     * @return Empty string if timeout occures, client's response including
     *         the end-of-response symbol (+, -)
     */
    QString _recv_result(int const expectedLength = 0);

// BUS HANDLING
    /*!
     * Compares the requested address with the local cache and
     * sends an address command to the client if necessary
     *
     * @param address new address
     * @param force if true command is send even if the cached value matches
     */
    void _set_address(unsigned int address, bool force = false);

    /*! number of records, that might be read additionally on block read
        in order to prevent a _set_length command */
    const static int _block_length_margin = 0;

    /*!
     * Compares the requested length with the local cache and
     * sends an address command to the client if necessary
     *
     * @param length new block read length
     * @param force if true command is send even if the cached value matches
     */
    void _set_length(unsigned int length, bool force = false);
};

#endif /* WBI_SERIAL_H_ */
