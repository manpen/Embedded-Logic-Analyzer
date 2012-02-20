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

#ifndef WBI_H_
#define WBI_H_

#include <exception>
#include <vector>

#include "BitVector.h"
#include <QDebug>


/** macro to output a warning (automatically adds filename and line
  @param x Message to be output (const char*) */
#define WBI_WARNING(x) qWarning() << __FILE__ << "at line" << __LINE__  << x;


/** @brief Default WBI exception

 Default WBI exception that is used as base class for more concrete exceptions,
 however this class might be used to indicate unknown errors.

 WBI driver may automatically repeat a failed communication. If those commands
 fail again the last error decides which exception is thrown. */
class WBIException : public std::exception {
    virtual const char* what() const throw() {return "WBIException";}
};

/** @brief  Exception thrown if a timeout occures while communication with client */
class WBITimeoutException : public WBIException {
    virtual const char* what() const throw() {return "WBITimeoutException";}
};

/** @brief Exception thrown if a response is invalid */
class WBIInvalidResponseException : public WBIException {
    virtual const char* what() const throw() {return "WBIInvalidResponseException";}
};

/** @brief Abstract WBI driver interface

  A concrete WBI driver is required to inherit from this class and implement
  all virtual methods. Thus any application using this drivier should use
  methods provided by this class only */
class WBI {
public:
// session handling
    /** Connect to wishbone bus
      @attention This function has to be called before any communication
      @return true on success, false on error */
    virtual bool connect() = 0;

    /** Disconnect from wishbone bus */
    virtual void disconnect() = 0;

// bus data
    /** Data width of the wishbone bus connected
      @return data width in bits. -1 if not supported by driver */
    virtual int getDataWidth() const {return -1;}

    /** Address width of the wishbone bus connected
      @return data width in bits. -1 if not supported by driver */
    virtual int getAddressWidth() const {return -1;}

// access
    /** Read data from one address. May throw WBIException on error.
      @param address address to read from */
    virtual BitVector read(unsigned int address)
        throw (WBITimeoutException, WBIInvalidResponseException) = 0;

    /** Read data from multiple sequential addresses.
      @param address first address to read from
      @param length number of sequential addresses to read from (ascending) */
    virtual std::vector<BitVector> readBlock(unsigned int address,
                                        unsigned int length)
        throw (WBITimeoutException, WBIInvalidResponseException) = 0;

    /** Write data to one address
      @param data buffer size atleast getDataWidth/8 Bytes.
      @param address address to write to */
    virtual void write(const BitVector & data, unsigned int address)
        throw (WBITimeoutException, WBIInvalidResponseException) = 0;

    /** Write data to multiple sequential addresses
      @param data holding values to write at address @p address + index
      @param address address to write to */
    virtual void writeBlock(const std::vector<BitVector> & data,
                            unsigned int address)
        throw (WBITimeoutException, WBIInvalidResponseException) = 0;
};

#endif /* WBI_H_ */
