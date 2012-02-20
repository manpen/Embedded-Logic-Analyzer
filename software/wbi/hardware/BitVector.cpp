#include"BitVector.h"
#include"math.h"
#include <algorithm>
#include <QDebug>
#include <QThread>

const BitVector BitVector::EMPTY(0);

/* Constructors & Destructor */
    BitVector::BitVector() {
        __initAndAlloc(8);
    }

    BitVector::BitVector(int size) {
        __initAndAlloc(size);
    }

    BitVector::BitVector(const BitVector & other)
    {
        __initAndAlloc(other.size());
        other.copy(_data);
    }

    BitVector::~BitVector() {
        __free();
    }

    BitVector & BitVector::operator= (BitVector const & other) {
        if (this == &other) return *this;

        if (bytes() != other.bytes()) {
            __free();
            __initAndAlloc(other.size());
        } else {
            _size = other.size();   // size might change never the less,
                                    // as bytes is only incremented every 8 bits ...
        }

        other.copy(_data);

        return *this;
    }

    void BitVector::__initAndAlloc(int size) {
        Q_ASSERT(size >= 0);

        _size = size;
        _bytes = int(ceil(size / 8.0));

        _data = NULL;

        _data = new char[_bytes + 2];
        Q_CHECK_PTR(_data);

        _data[0] = _data[bytes()+1] = 0;    // padding -> less conditionals -> faster
        _data++;

      //  qWarning() << "allo " << QThread::currentThreadId() << ": " << (void*) _dummy;
    }

    void BitVector::__free() {
        delete[] (_data-1);
        _data = NULL;
    }

    void BitVector::resize(unsigned int newSize) {
        int newBytes = int(ceil(newSize / 8.0));

        if (newBytes == bytes() || (newBytes < bytes() && newBytes+4 >= bytes())) {
            // size didn't change (or shrunk only be max. 4 bytes, that can
            // be wasted for faster execution)
            _size = newSize;
            return;
        } else {
            // alloc new area
            char* oldData = _data;
            int oldBytes = bytes();

            __initAndAlloc(newSize);

            std::copy(oldData, oldData + qMin(oldBytes, bytes()), _data);

            for (int i = oldBytes; i < bytes(); i++)
                _data[i] = 0;

            delete[] (oldData - 1);
        }

        if (size() % 8) {_data[bytes()-1] &= ~(0xFF << (size() % 8));}
    }

/* Castings */
    void BitVector::copy(char* dest) const {
        std::copy(_data, _data+bytes(), dest);
    }


    unsigned char BitVector::toChar() const {
        return (unsigned char) _data[0];
    }

    unsigned int BitVector::toInt() const {
        unsigned int result = 0;
        for(int i=bytes()-1; i>= 0; i--)
            result = (result << 8) | ((unsigned int)_data[i] & 0xFF);

        return result;
    }

    QString BitVector::toString(unsigned int base, bool leftExpand) const {
        QString result;
        Q_ASSERT(base == 2 || base == 16);

        if (base == 16) {
            result.reserve((int) ceil(size() / 4.0));
            for(int i=bytes()-1; i>=0; i--) {
                for(int c=1; c>=0; c--) {
                    if (8*i + 4*c > size()) continue;
                    int digit = (_data[i] >> (4 * c)) & 0x0F;
                    if (!leftExpand && !digit) continue;
                    leftExpand = true;

                    result.append(QString::number(digit, 16 ));
                }
            }
        } else {
            result.reserve(size());
            int bits = size() % 8;
            if (!bits) bits=8;
            for(int i=bytes()-1; i>=0; i--) {
                for(int c=bits; c; c--) {
                    bool current = (_data[i] >> (c-1)) & 0x1;
                    if (!leftExpand && !current) continue;
                    leftExpand = true;
                    result.append(current ? '1' : '0');
                }
                bits = 8;
            }
        }

        result.squeeze();

        return result;
    }

    std::vector<BitVector> BitVector::split(unsigned int chunkSize) const {
        std::vector<BitVector> result;

        int begin = 0;
        while(begin < size()) {
            BitVector tmp = slice(begin, qMin(int(begin+chunkSize), size()));
            result.push_back(tmp);
            begin += chunkSize;
        }

/*        if (begin+int(chunkSize) != size()) {
            result.push_back(slice(begin));
        } */

        return result;
    }

/* Generators */
    BitVector BitVector::fromInt(unsigned int value, int size) {
        if (size < 0) size = sizeof(value) * 8;
        BitVector result(size);

        if (size < (int) sizeof(value)*8)
            value &= (unsigned int)(1<<size) - 1;

        for(int i=0; i<result.bytes(); i++) {
            result._data[i] = (unsigned char)value;
            value >>= 8;
        }

        return result;
    }

    BitVector BitVector::fromCharArray(char* buffer, unsigned int size) {
        BitVector result(size);
        std::copy(buffer, buffer + result.bytes(), result._data);
        if (size % 8) {
            result._data[result.bytes() - 1]  &= ~(0xFF << (result.size() % 8));
        }

        return result;
    }

    BitVector BitVector::fromString(QString const & value, int base, int size, bool* ok) {
        Q_ASSERT(size > 0 || value.size());
        Q_ASSERT(base == 2 || base == 16);

        if (size < 0)
            size = value.size() * (base == 2 ? 1 : 4);

        BitVector result(size);

        const int charsPerByte = base == 2 ? 8 : 2;

        bool success = true;
        for(int i = 0; i < result.bytes(); i++) {
            int len = charsPerByte;
            int start = value.size() - charsPerByte*(i + 1);
            if (start > -charsPerByte) {
                if (start < 0) {
                    len+= start;
                    start = 0;
                }

                result._data[i] = value.mid(start, len).toInt(&success, base);
            } else {
                result._data[i] = 0;
            }

            if (!success) {

                break;
            }
        }

        if (result.size() % 8) {
            // set unused bits in the last byte to 0
            result._data[result.bytes()-1] &= ~(0xFF << (result.size() % 8));
        }

        if (ok != NULL)
            *ok = success;


        return result;
    }

    BitVector BitVector::fromVector(const std::vector<BitVector> &vec) {
        unsigned int size = 0;

        for(int i=0; i < (int) vec.size(); i++)
            size += vec.at(i).size();

        BitVector result(size);

        int begin = 0;
        for(int i=0; i < (int) vec.size(); i++) {
            result.fill(vec.at(i), begin);
            begin += vec.at(i).size();
        }

        return result;
    }

/* Compare */
     bool BitVector::operator==(BitVector const & other) const {
         if (other.size() != size())
             return false;

         for(int i=bytes()-1; i>=0; i--)
             if (other._data[i] != _data[i])
                 return false;

         return true;
     }

     bool BitVector::operator==(bool other) const {
         const char ref = other ? 0xFF : 0x00;
         for(int i=bytes()-1; i>=0; i--)
             if (_data[i] != ref)
                 return false;

         return true;
     }

     bool BitVector::operator!=(BitVector const & other) const {
         return !(*this == other);
     }

     bool BitVector::operator!=(bool other) const {
         return !(*this == other);
     }

/* Altering data */
    void BitVector::fill(bool value, int begin, int end) {
        if (begin < 0) begin += size();
        if (end < 0) end += size();
        if (begin==end) return;

        Q_ASSERT(begin >= 0);
        Q_ASSERT(end >= begin && end < size());

        int _begin = begin / 8;
        end++;
        const int
            _beginMod = begin % 8,
            _end = end / 8,
            _endMod = end % 8;


        if (_beginMod) {
            if (value) _data[_begin++] |= 0xFF << _beginMod;
            else       _data[_begin++] &= ~(0xFF <<_beginMod);
        }

        if (_endMod) {
            if (value) _data[_end] |= 0xFF >> (8-_endMod);
            else       _data[_end] &= (0xFF << _endMod);
        }

        for(int i=_begin; i < _end; i++)
            _data[i] = value ? 0xFF : 0x00;
    }

    void BitVector::fill(BitVector const & pattern, int begin, int end) {
    // check parameters
        if (begin < 0) begin += size();
        if (end < 0) end += size();
        if (begin==end) return;

        Q_ASSERT(begin >= 0);
        Q_ASSERT(end >= begin && end < size());

        const int length = qMin(pattern.size(), end - begin + 1);

    // calculate offsets
        const int byteOffset = begin / 8;
        const int bitsOffset = begin % 8;
        int bitsCopied, currentIndex=0;

        if (0 == bitsOffset) {
            // if there is no bit-offset, we can easily copy byte-wise
            for(currentIndex = 0; currentIndex < length/8; currentIndex++) {
                _data[byteOffset + currentIndex] = pattern._data[currentIndex];
            }

            bitsCopied = 8*(length/8);
        } else {
            // okay, there's a bit shift, so we have to take two bytes from the
            // fill-pattern at a time

            // first Byte
            // blank out for new data
            if (length+bitsOffset > 8)
                _data[byteOffset] &= ~(0xFF << bitsOffset);
            else
                // need to crop from left and right
                _data[byteOffset] &= ~((0xFF << bitsOffset) & (0xFF >> (8-length-bitsOffset)));

            _data[byteOffset] |= pattern._data[0] << bitsOffset;
            bitsCopied = 8 - bitsOffset;

            // copy all bytes but the last if it's incomplemente
            for(currentIndex=1; currentIndex <= (length - bitsCopied)/8; currentIndex++) {
                int twoBytes = ((int(pattern._data[currentIndex]) & 0xFF) << 8) | (pattern._data[currentIndex-1] & 0xFF);
                _data[byteOffset+currentIndex] = twoBytes >> (8-bitsOffset);
            }

            bitsCopied += 8*((length-bitsCopied)/8);
        }

        int left = length - bitsCopied;
        if (left > 0) {
            // blank lower bits of last byte
            _data[byteOffset+currentIndex] &= 0xFF << left;

            int twoBytes = ((int(pattern._data[currentIndex]) & 0xFF) << 8) | (pattern._data[currentIndex-1] & 0xFF);

            _data[byteOffset+currentIndex] |= (twoBytes >> (8 - bitsOffset)) & (0xFF >> (8-left));
        }
    }

    void BitVector::setBit(int index, bool value) {
        Q_ASSERT(index >= 0 && index < size());

        if (value)
            _data[index/8] |= (1 << (index % 8));
        else
            _data[index/8] &= ~(1 << (index % 8));
    }

    bool BitVector::getBit(int index) const {
        Q_ASSERT(index >= 0 && index < size());
        bool tmp = _data[index/8] & (1 << (index % 8));
        return tmp;
    }

    unsigned int BitVector::countHigh() const {
        unsigned int result = 0;
        for (int i = 0; i < bytes(); i++) {
            char current = _data[i];

            for(int t = 0; t < 7 && current; t++) {
                if (current & 0x1) result++;
                current >>= 1;
            }
        }

        return result;
    }


/* Indexing */
    bool BitVector::operator[] (unsigned int i) const {
        Q_ASSERT(i < (unsigned int) size());

        return bool(_data[i / 8] & (1 << (i % 8)));
    }

    bool BitVector::operator[] (int i) const {
        if (i < 0) i += size();
        Q_ASSERT(i >= 0 && i < size());

        return bool(_data[i / 8] & (1 << (i % 8)));
    }

    BitVector BitVector::slice(int begin, int end) const {
        if (begin < 0) begin += size();
        if (end < 0) end += size();

        Q_ASSERT(begin >= 0);
        Q_ASSERT(end > begin && end <= size());

        const int length = end - begin;
        const int offset = begin % 8;
        const int byteOffset = begin / 8;

        BitVector result(length);
        if (offset) {
            // need to shift -> slower
            for(int i = 0; i < result.bytes(); i++) {
                result._data[i] = int(((int(_data[i+1+byteOffset]) & 0xFF) << 8) | (int(_data[i+byteOffset]) & 0xFF)) >> offset;
            }
        } else {
            // plain copy
            for(int i = 0; i < result.bytes(); i++)
                result._data[i] = _data[i + byteOffset];
        }

        if (result.size() % 8) {
            // set unused bits in the last byte to 0
            result._data[result.bytes()-1] &= ~(0xFF << (result.size() % 8));
        }

        return result;
    }

 /* BitWise-Operators */
    BitVector BitVector::operator~() const {
        BitVector result(size());
        for(int i=bytes()-1; i>=0; i--)
            result._data[i] = ~_data[i];

        if (size() % 8) {
            // set unused bits in the last byte to 0
            result._data[result.bytes()-1] &= ~(0xFF << ((size() % 8)));
        }

        return result;
    }

    BitVector & BitVector::operator|=(const BitVector & other) {
        Q_ASSERT(other.size() == size());

        for(int i=bytes()-1; i>=0; i--)
            _data[i] |= other._data[i];

        return *this;
    }

    BitVector & BitVector::operator&=(const BitVector & other) {
        Q_ASSERT(other.size() == size());

        for(int i=bytes()-1; i>=0; i--)
            _data[i] &= other._data[i];

        return *this;
    }

    BitVector & BitVector::operator^=(const BitVector & other) {
        Q_ASSERT(other.size() == size());

        for(int i=bytes()-1; i>=0; i--)
            _data[i] ^= other._data[i];

        return *this;
    }

    BitVector & BitVector::operator|=(bool other) {
        const char byte = other ? 0xFF : 0x00;
        for(int i=bytes()-1; i>=0; i--)
            _data[i] |= byte;

        if (size() % 8) {
            // set unused bits in the last byte to 0
            _data[bytes()-1] &= ~(0xFF << (size() % 8));
        }

        return *this;
    }

    BitVector & BitVector::operator&=(bool other) {
        const char byte = other ? 0xFF : 0x00;
        for(int i=bytes()-1; i>=0; i--)
            _data[i] &= byte;

        if (size() % 8) {
            // set unused bits in the last byte to 0
            _data[bytes()-1] &= ~(0xFF << (size() % 8));
        }

        return *this;
    }

    BitVector & BitVector::operator^=(bool other) {
        const char byte = other ? 0xFF : 0x00;
        for(int i=bytes()-1; i>=0; i--)
            _data[i] ^= byte;

        if (size() % 8) {
            // set unused bits in the last byte to 0
            _data[bytes()-1] &= ~(0xFF << (size() % 8));
        }

        return *this;
    }


    BitVector & BitVector::operator<<=(int v) {
        if (!v) return *this;

        if (v < 0) {
            *this >>= -v;
            return *this;
        }

        if (v > size()) {
            // that's faster ..
            fill(false);
            return *this;
        }

        const int shift = v / 8;
        const int offset = v % 8;

        for(int i=bytes() - shift - 1; i >= 0; i--) {
            int twoBytes = (int(_data[i] & 0xFF) << 8) | (_data[i-1] & 0xFF);
            _data[i+shift] = twoBytes >> (8 - offset);
        }

        for(int i=shift-1; i>=0; i--)
            _data[i] = 0;

        if (size() % 8) {
            // set unused bits in the last byte to 0
            _data[bytes()-1] &= ~(0xFF << ((size() % 8)));
        }

        return *this;
    }

    BitVector & BitVector::operator>>=(int v) {
        if (v < 0) {
            *this <<= -v;
            return *this;
        }

        if (v > size()) {
            // that's faster ..
            fill(false);
            return *this;
        }

        const int shift = v / 8;
        const int offset = v % 8;

        for(int i = shift; i < bytes(); i++) {
            int twoBytes = (int(_data[i+1] & 0xFF) << 8) | (_data[i] & 0xFF);
            _data[i-shift] = twoBytes >> offset;
        }

        for(int i = bytes() - shift; i < bytes(); i++)
            _data[i] = 0;

        if (size() % 8) {
            // set unused bits in the last byte to 0
            _data[bytes()-1] &= ~(0xFF << ((size() % 8)));
        }

        return *this;
    }

/* Tools */
    int BitVector::crc(int generator, int degree, int initial, bool fullNibble) const {
        Q_ASSERT(degree > 0 && degree < int(sizeof(int)*8));

        int result = initial;

        int bits = size() % 8;
        if (!bits)
            bits=8;
        else if(fullNibble && bits % 4 != 0)
            bits = bits < 4 ? 4 : 8;

        for(int i = bytes() - 1; i >= 0; i--) {
            unsigned char current = _data[i] << (8-bits);
            for(int c = bits; c; c--) {
                result <<= 1;
                if (bool(current & 0x80) != bool(result & (1<<degree)))
                    result ^= generator;

                current <<= 1;
            }

            bits = 8;
        }

        return result & ((1 << (degree))-1);
    }

#define USE_COMP(op, other) BitVector result(*this); result op other; return result;
    BitVector BitVector::operator|(const BitVector & other) const {USE_COMP(|=, other)}
    BitVector BitVector::operator&(const BitVector & other) const {USE_COMP(&=, other)}
    BitVector BitVector::operator^(const BitVector & other) const {USE_COMP(^=, other)}
    BitVector BitVector::operator|(bool other) const {USE_COMP(|=, other)}
    BitVector BitVector::operator&(bool other) const {USE_COMP(&=, other)}
    BitVector BitVector::operator^(bool other) const {USE_COMP(^=, other)}
    BitVector BitVector::operator<<(int v) const {USE_COMP(<<=, v)}
    BitVector BitVector::operator>>(int v) const {USE_COMP(>>=, v)}
