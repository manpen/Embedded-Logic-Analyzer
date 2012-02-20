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

#ifndef BITVECTOR_H
#define BITVECTOR_H

#include <QString>
#include <QDebug>
#include <QMetaType>
#include <vector>

/**
  @brief BitVector of arbitrary but constant length

  On construction of the instance the length of the BitVector is specified
  and cannot be altered later. Common bitwise operations can be performed
  on each instance.

  A class instance uses sizeof(int) + ceil(size() / 8) bytes of memory and
  thus is very inexpensive.
  */
class BitVector {
public:
    static const BitVector EMPTY;

/* Constructors & Destructor */
    BitVector();

    /** Constructor of BitVector
      @param size Size of BitVector in Bits */
    explicit BitVector(int size);

    /** Copy-Constructor of BitVector
      @param other Instance to copy */
    BitVector(const BitVector & other);

    /** Destructor */
    virtual ~BitVector();

 /* Size */
    /** Vector size in Bits */
    inline int size() const {return _size;}

    /** Vector size in Bytes */
    inline int bytes() const {return _bytes;}

 /* Generators */
    /** Generate a BitVector from integer.
      @param value Value to create from
      @param size  Length of vector in bits. If bigger than int,
        missing bits will be filled with 0 */
    static BitVector fromInt(unsigned int value, int size=-1);

    /** Generate a BitVector from char*
      @attention Ensure buffer is big enough
      @param buffer First byte of buffer to create from
      @param size   Length of vector in bits */
    static BitVector fromCharArray(char* buffer, unsigned int size);

    /** Generate a BitVector from QString.
      @attention if size is omitted, a non-empty value string is required
      @param value Value to create from (First character is most significant)
      @param base  Base of QString. Supported are 2 and 16
      @param size  Length of vector in bits. If bigger than value,
        missing bits will be filled with 0
      @param ok    Indicates whether conversion was successful */
    static BitVector fromString(QString const & value, int base=16,
                                int size=-1, bool *ok = NULL);

    /** Concatenate BitVectors provided ascendingly.
      @param vec Vector of BitVectors */
    static BitVector fromVector(std::vector<BitVector> const & vec);

 /* Castings */
    /** Copy data data to destination
      @param dest Pointer to the first byte of the buffer
      @attention The buffer must hold atleast @ref size() bytes */
    void copy(char* dest) const;

    /** Interpret vector as 8bit char. If vector is shorter,
      missing bits are filled up with 0. If longer the least
      significant bits are returned */
    unsigned char toChar() const;

    /** Interpret vector as sizeof(uint)-byte number. If vector
      is shorter, missing bits are filled up with 0. If longer
      the least significant bits are returned */
    unsigned int  toInt() const;

    /** Returns a representation of the vector in little endian
      (i.e. the most significant digit is the first char).
      @param base Base of representation. Only 2 and 16 are supported
      @param leftExpand If true the results length depends only on the vectors
        size. If false all left 0 are truncated */
    QString toString(unsigned int base = 16, bool leftExpand = true) const;

    /** Splits the BitVector in smaller BitVectors each with chunkSize-bits.
      The last vector may be smaller. */
    std::vector<BitVector> split(unsigned int chunkSize) const;

 /* Indexing */
    /** Returns a copy of slice of the vector.
      @param begin Begin of the slice (negative numbers are
        interpreted as @ref size() - begin)
      @param end End of the slice (negative numbers are
        interpreted as @ref size() - end) */
    BitVector slice(int begin=0, int end=-1) const;

    /** Returns the value of the i-th bit.
      Negative values of i are interpreted as @ref size() - \c i */
    bool operator[] (int i) const;

    /** Returns the value of the i-th bit. */
    bool operator[] (unsigned int i) const;

/* Manipulation */
    /** Override a range (both indicies including) of bits
      @param begin Beginning of range (negative numbers are interpreted
        as @ref size() - begin)
      @param end End of range (negative numbers are interpreted
        as @ref size() - end)
      @param value New value */
    void fill(bool value = false, int begin=0, int end=-1);

    /** Override a range (both indicies including) of bits with the
        pattern provided.
      @param pattern New pattern (bits exceeding the destination vector
        are ignored)
      @param begin Beginning of the range in bits (negative numbers are
        interpreted as @ref size() - num)
      @param end End of range (negative numbers are
        interpreted as @ref size() - end); if range is longer
                 than pattern.size(), the shorter range is used. */
    void fill(BitVector const & pattern, int begin=0, int end=-1);

    /** Get single bit
      @param index Index of bit **/
    bool getBit(int index) const;

    /** Set single bit
      @param index Index of bit
      @param value New value of bit */
    void setBit(int index, bool value = true);

    /** Resize. If size is increased, new bits are filled with 0. If size is
      decreased by a small amount, the unused memory might not be freed for
      performance reasons.
      @param size New size */
    void resize(unsigned int size);

/* Tools */
    /** Calculate CRC with a polynom of degree < sizeof(int)*8.
      @param generator  Generator-Polynom
      @param degree     Degree of the polynom (number of bits - 1).
      @param initial    Inital code. Can be used to concatenate CRC calculations
      @param fullNibble if size%4 != 0 append most significant 0-bits.
      @return crc code */
    int crc(int generator, int degree, int initial=0, bool fullNibble = false) const;

/* Bitwise operations */
    /** Calculate complement of the vector (i.e. generate new object with all
      bits flipped) */
    BitVector operator~() const;

    /** Bitwise OR
      @attention Consider using compound assignment operators as they are faster
      @param other Second Operand */
    BitVector operator|(const BitVector & other) const;

    /** Bitwise AND
      @attention Consider using compound assignment operators as they are faster
      @param other Second Operand */
    BitVector operator&(const BitVector & other) const;

    /** Bitwise Exclusive OR
      @attention Consider using compound assignment operators as they are faster
      @param other Second Operand */
    BitVector operator^(const BitVector & other) const;

    /** Bitwise OR
      @attention Consider using compound assignment operators as they are faster
      @param other Second Operand (single boolean is applied to each bit) */
    BitVector operator|(bool other) const;

    /** Bitwise AND
      @attention Consider using compound assignment operators as they are faster
      @param other Second Operand (single boolean is applied to each bit) */
    BitVector operator&(bool other) const;

    /** Bitwise Exclusive OR
      @attention Consider using compound assignment operators as they are faster
      @param other Second Operand (single boolean is applied to each bit) */
    BitVector operator^(bool other) const;

    /** Left-Shift
      @attention Consider using compound assignment operators as they are faster
      @param v Number of bits to shift (negative numbers are interpreted
        as a Right-Shift) */
    BitVector operator<<(int v) const;

    /** Right-Shift
      @attention Consider using compound assignment operators as they are faster
      @param v Number of bits to shift (negative numbers are interpreted
        as a Left-Shift) */
    BitVector operator>>(int v) const;

    /** Compare with another BitVector
      @param other BitVector to compare with
      @return true if size and all bits match, false otherwise */
    bool operator==(BitVector const & other) const;

    /** Compare each bit with other.
      @param other Bit to compare with
      @return true if all bits equal other, false otherwise */
    bool operator==(bool other) const;

    /** Compare with another BitVector
      @param other BitVector to compare with
      @return true if size or bits do NOT match, false otherwise */
    bool operator!=(BitVector const & other) const;

    /** Compare each bit with other.
      @param other Bit to compare with
      @return true if some (or all) bits ARE NOT equal other, false otherwise */
    bool operator!=(bool other) const;

    /** Count number of bits that are High */
    unsigned int countHigh() const;

/* Compound assignment operators */
    /** Bitwise OR + Assignment
      @param other Second Operand */
    BitVector & operator|=(const BitVector & other);

    /** Bitwise AND + Assignment
      @param other Second Operand */
    BitVector & operator&=(const BitVector & other);

    /** Bitwise Exclusive OR + Assignment
      @param other Second Operand */
    BitVector & operator^=(const BitVector & other);

    /** Bitwise OR + Assignment
      @param other Second Operand (single boolean is applied to each bit) */
    BitVector & operator|=(bool other);

    /** Bitwise AND + Assignment
      @param other Second Operand (single boolean is applied to each bit) */
    BitVector & operator&=(bool other);

    /** Bitwise Exclusive OR + Assignment
      @param other Second Operand (single boolean is applied to each bit) */
    BitVector & operator^=(bool other);

    /** Left-Shift + Assignment
      @param v Second Operand */
    BitVector & operator<<=(int v);

    /** Right-Shift + Assignment
      @param v Second Operand */
    BitVector & operator>>=(int v);

    /** Copy vector provided to this vector (default assign-operator behaviour,
      in some cases more efficient)
      @param other vector to copy from */
    BitVector & operator= (BitVector const & other);

protected:
    /** stores size in bits. only access via getter @ref size() */
    int  _size;

    /** stores size in bytes. only access via getter @ref bytes() */
    int  _bytes;

    /** pointer to first element of bit-buffer */
    char* _data;

private:
    /** Allocate memory and setup _size and _bytes.
      @param size Length of vector in bits **/
    void __initAndAlloc(int size);

    /** Deallocate memory and clean up _size and _bytes. */
    void __free();
};

Q_DECLARE_METATYPE(BitVector)

#endif // BITVECTOR_H
