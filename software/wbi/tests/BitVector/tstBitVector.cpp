#include <QtCore/QString>
#include <QtTest/QtTest>
#include <BitVector.h>
#include <math.h>

class tstBitVector : public QObject
{
    Q_OBJECT

public:
    tstBitVector();

private Q_SLOTS:
    void initTestCase();
    void cleanupTestCase();

    /** checks the following constructors:
        - default constructor
        - size constructor
        - copy constructor */
    void constructor();
    void constructor_data();

    /** checks the following castings:
        - int -> int
        - int -> String
        - int -> char
        - int -> char* (copy)
        - String(hex, not-justified) -> int
        - String(hex, justified) -> int
        - String(bin, not-justified) -> int
        - String(bin, justified) -> int
        - char* -> int
    */
    void cast();
    void cast_data();

    /** checks the following compares:
       - BitVector == BitVector
       - BitVector == Bool

       - BitVector != BitVector
       - BitVector != Bool
    */
    void compares();


    /** check the following bitswise accesses:
      - getBit()
      - setBit()
      - [] */
    void bitwiseAccess();

    /** Logic Operators:
      - |, &, ^   with BitVector
      - |, &, ^   with bool
      - |=, &=, ^= with BitVector
      - |=, &=, ^= with bool
      - ~
      - ~= */
    void logicOperators();

    /** Fill:
      - fillBitVector
      */
    void fillBitVector();
    void fillBitVector_data();

    /** Fill
      -fill(Bool)
      */
    void fillBool();
    void fillBool_data();

    /** Shift
      - <<, >>      with pos and neg. numbers
      - <<=, >>=    with pos and neg. numbers */
    void shift();
    void shift_data();

    /** Slice */
    void slice();
    void slice_data();

    void crc();
    void crc_data();

};

tstBitVector::tstBitVector() {}
void tstBitVector::initTestCase(){}
void tstBitVector::cleanupTestCase(){}

// constructor
    void tstBitVector::constructor()
    {
        QFETCH(BitVector, vec);
        QFETCH(int, size);
        QFETCH(int, bytes);


        QCOMPARE(vec.size(), size);
        QCOMPARE(vec.bytes(), bytes);
    }
    void tstBitVector::constructor_data()
    {
        QTest::addColumn<BitVector>("vec");
        QTest::addColumn<int>("size");
        QTest::addColumn<int>("bytes");

        QTest::newRow("4") << BitVector()  << 8 << 1;
        QTest::newRow("4") << BitVector(4) << 4 << 1;
        QTest::newRow("5") << BitVector(5) << 5 << 1;
        QTest::newRow("8") << BitVector(8) << 8 << 1;
        QTest::newRow("9") << BitVector(9) << 9 << 2;
        QTest::newRow("16") << BitVector(16) << 16 << 2;
        QTest::newRow("129") << BitVector(129) << 129 << 17;
        QTest::newRow("129") << BitVector(BitVector(129)) << 129 << 17;    }

// cast
    void tstBitVector::cast() {
        QFETCH(unsigned int, value);
        QFETCH(int, size);
        QFETCH(unsigned int, result);

        char buffer[sizeof(value)] = {0};

        // int -> int
        BitVector vec = BitVector::fromInt(value, size);
        QCOMPARE(vec.toInt(), (unsigned int) result);

        // int -> string
        QString num = QString::number(result, 16).rightJustified(ceil(vec.size()/4.0), '0');
        QCOMPARE(vec.toString(), num);

        // int -> char
        QCOMPARE(vec.toChar(), (unsigned char) result);

        // int -> char*
        vec.copy(buffer);
        unsigned int tmp = result;
        for(int i=0; i < (int)sizeof(value); i++) {
            QCOMPARE(((unsigned int)buffer[i])&0xFF, tmp&0xFF);
            tmp >>= 8;
        }


        // string -> int
        bool okay = true;
        vec = BitVector::fromString(QString::number(value, 16).rightJustified(vec.bytes()*2, '0'), 16, size, & okay);
        QVERIFY(okay);
        QCOMPARE(vec.toInt(), result);

        vec = BitVector::fromString(QString::number(value, 16), 16, size, & okay);
        QVERIFY(okay);
        QCOMPARE(vec.toInt(), result);

        vec = BitVector::fromString(QString::number(value, 2).rightJustified(vec.bytes()*8, '0'), 2, size, & okay);
        QVERIFY(okay);
        QCOMPARE(vec.toInt(), result);

        vec = BitVector::fromString(QString::number(value, 2), 2, size, &okay);
        QVERIFY(okay);
        QCOMPARE(vec.toInt(), result);

        // char * -> int
        if (size > 0) {
            tmp = value;
            for(int i=0; i < (int)sizeof(value); i++) {
                buffer[i] = tmp & 0xFF;
                tmp >>= 8;
            }

            vec = BitVector::fromCharArray(buffer, size);
            QCOMPARE(vec.toInt(), result);
       }
    }
    void tstBitVector::cast_data() {
        QTest::addColumn<unsigned int>("value");
        QTest::addColumn<int>("size");
        QTest::addColumn<unsigned int>("result");

        QTest::newRow("0 sizeof(int)")   << (uint)0   << (int)sizeof(int)*8 << (uint)0;
        QTest::newRow("1 sizeof(int)")   << (uint)1   << (int)sizeof(int)*8 << (uint)1;
        QTest::newRow("256 sizeof(int)") << (uint)256 << (int)sizeof(int)*8 << (uint)256;

        QTest::newRow("0 -1")   << (uint)0   << -1 << (uint)0;
        QTest::newRow("1 -1")   << (uint)1   << -1 << (uint)1;
        QTest::newRow("256 -1") << (uint)256 << -1 << (uint)256;

        QTest::newRow("0   7")   << (uint)0   << 7 << (uint)0;
        QTest::newRow("255 7")   << (uint)255 << 7 << (uint)127;

        QTest::newRow("0 8")   << (uint)0   << 8 << (uint)0;
        QTest::newRow("1 8")   << (uint)1   << 8 << (uint)1;
        QTest::newRow("256 8") << (uint)256 << 8 << (uint)0;

        QTest::newRow("0 9")   << (uint)0   << 9 << (uint)0;
        QTest::newRow("1 9")   << (uint)1   << 9 << (uint)1;
        QTest::newRow("256 9") << (uint)256 << 9 << (uint)256;
        QTest::newRow("512 9") << (uint)512 << 9 << (uint)0;
    }

// compares
    void tstBitVector::compares() {
        QVERIFY( (BitVector::fromInt(0) == BitVector::fromInt(0)) );
        QVERIFY(!(BitVector::fromInt(0) == BitVector::fromInt(1)) );
        QVERIFY(!(BitVector::fromInt(0,8) == BitVector::fromInt(0,9)) );
        QVERIFY( (BitVector::fromInt(512,8) == BitVector::fromInt(0,8)) );

        QVERIFY( (BitVector::fromInt(0) == false) );
        QVERIFY( !(BitVector::fromInt(0) == true) );

        QVERIFY( !(BitVector::fromInt(1) == false) );
        QVERIFY( !(BitVector::fromInt(1) == true) );

        QVERIFY( !(BitVector::fromInt(0xFF, 8) == false) );
        QVERIFY( (BitVector::fromInt(0xFF, 8) == true) );


        QVERIFY(!(BitVector::fromInt(0) != BitVector::fromInt(0)) );
        QVERIFY( (BitVector::fromInt(0) != BitVector::fromInt(1)) );
        QVERIFY( (BitVector::fromInt(0,8) != BitVector::fromInt(0,9)) );
        QVERIFY(!(BitVector::fromInt(512,8) != BitVector::fromInt(0,8)) );

        QVERIFY(!(BitVector::fromInt(0) != false) );
        QVERIFY( (BitVector::fromInt(0) != true) );

        QVERIFY( (BitVector::fromInt(1) != false) );
        QVERIFY( (BitVector::fromInt(1) != true) );

        QVERIFY( (BitVector::fromInt(0xFF, 8) != false) );
        QVERIFY(!(BitVector::fromInt(0xFF, 8) != true) );
    }

// bitwise access
    void tstBitVector::bitwiseAccess() {
        int indicies[] = {0, 5, 0, 3, 0, 9, 16, 16, 16, 29, 23, 21, 11, 29, 11, 23, 12};
        int values[]   = {1, 1, 0, 1, 1, 1, 0,  1,  1,  1,  0,  1,  1,  0,  0,  1,  1};
        int ref = 0xA5A5A5A5;
        const unsigned int mask = -1;

        BitVector vec = BitVector::fromInt(ref, 30);
        for(int i=0; i<(int)(sizeof(indicies)/sizeof(int)); i++) {
            int ind = indicies[i];

            vec.setBit(ind, bool(values[i]));

            ref |= 1 << ind;
            if (!values[i])
                ref ^= 1 << ind;

            QCOMPARE(vec.toInt(), (unsigned int) ref & (mask >> (sizeof(mask)*8 - vec.size())) );
            for(int t=0; t< (int)vec.size(); t++) {
                QVERIFY(vec.getBit(t) == bool(ref & (1<<t)));
                QVERIFY(vec[t] == bool(ref & (qint64(1)<<t)));
            }
        }
    }

 // Logic Operators
    void tstBitVector::logicOperators() {
        int sizes[] = {8, 16, 24, 32, 4, 7, 9, 15, 17, 23, 25, 29};
        unsigned int pattern[] = {0x00000000,
                                  0x55555555,
                                  0xAAAAAAAA,
                                  0xFFFFFFFF,
                                  0x12345678,
                                  0x7a5b9c10};

        for(int i=0; i < (int)(sizeof(sizes)/sizeof(sizes[0])); i++) {
            const int size=sizes[i];
            const unsigned int mask = 0xFFFFFFFF >> (32 - size);
            for(int a=0; a<(int)(sizeof(pattern)/sizeof(pattern[0])); a++) {
                const unsigned int pattern1 = pattern[a];

            // BitVector vs. BitVector
                for(int b=0; b<(int)(sizeof(pattern)/sizeof(pattern[0])); b++) {
                    const unsigned int pattern2 = pattern[b];
                    const unsigned int
                            ref_or  = mask & (pattern1 | pattern2),
                            ref_and = mask & (pattern1 & pattern2),
                            ref_xor = mask & (pattern1 ^ pattern2),
                            ref_com = mask & (~pattern1);


                    // Combound operators
                    BitVector vec2 = BitVector::fromInt(pattern2, size);
                    BitVector vec1 = BitVector::fromInt(pattern1, size);
                    vec1 |= vec2;
                    QCOMPARE(vec1.toInt(), ref_or);
                    QCOMPARE(vec2.toInt(), pattern2 & mask);

                    vec1 = BitVector::fromInt(pattern1, size);
                    vec1 &= vec2;
                    QCOMPARE(vec1.toInt(), ref_and);
                    QCOMPARE(vec2.toInt(), pattern2 & mask);

                    vec1 = BitVector::fromInt(pattern1, size);
                    vec1 ^= vec2;
                    QCOMPARE(vec1.toInt(), ref_xor);
                    QCOMPARE(vec2.toInt(), pattern2 & mask);

                    // Operators
                    QCOMPARE((BitVector::fromInt(pattern1, size) | BitVector::fromInt(pattern2, size)).toInt(), ref_or);
                    QCOMPARE((BitVector::fromInt(pattern1, size) & BitVector::fromInt(pattern2, size)).toInt(), ref_and);
                    QCOMPARE((BitVector::fromInt(pattern1, size) ^ BitVector::fromInt(pattern2, size)).toInt(), ref_xor);
                    QCOMPARE((~BitVector::fromInt(pattern1, size)).toInt(),                                     ref_com);
                }

           // BitVector vs. Bool
                for(int b=0; b < 2; b++) {
                    bool pattern2 = bool(b);
                    int num_pattern2 = 0;
                    if (pattern2) num_pattern2 = ~num_pattern2;

                    const unsigned int
                            ref_or  = mask & (pattern1 | num_pattern2),
                            ref_and = mask & (pattern1 & num_pattern2),
                            ref_xor = mask & (pattern1 ^ num_pattern2);

                    // Combound operators
                    BitVector vec1 = BitVector::fromInt(pattern1, size);
                    vec1 |= pattern2;
                    QCOMPARE(vec1.toInt(), ref_or);

                    vec1 = BitVector::fromInt(pattern1, size);
                    vec1 &= pattern2;
                    QCOMPARE(vec1.toInt(), ref_and);

                    vec1 = BitVector::fromInt(pattern1, size);
                    vec1 ^= pattern2;
                    QCOMPARE(vec1.toInt(), ref_xor);

                    // Operators
                    QCOMPARE((BitVector::fromInt(pattern1, size) | pattern2).toInt(), ref_or);
                    QCOMPARE((BitVector::fromInt(pattern1, size) & pattern2).toInt(), ref_and);
                    QCOMPARE((BitVector::fromInt(pattern1, size) ^ pattern2).toInt(), ref_xor);
                }
            }
        }
    }

 // Fill
    void tstBitVector::fillBool() {
        QFETCH(unsigned int, init);
        QFETCH(int, size);
        QFETCH(bool, fill);
        QFETCH(int, begin);
        QFETCH(int, end);

        //qDebug() << init << fill << begin << end;

        BitVector vec = BitVector::fromInt(init, size);
        vec.fill(fill, begin, end);

        const unsigned int mask = 0xFFFFFFFF;
        unsigned int ref = init;
        if (fill) {
            ref |= (mask << begin) & (mask >> (31-end));
        } else {
            ref &= ~((mask << begin) & (mask >> (31-end)));
        }

        QCOMPARE(vec.toInt(), ref & (mask >> (32 - size)));
    }
    void tstBitVector::fillBool_data() {
        QTest::addColumn<unsigned int>("init");
        QTest::addColumn<int>("size");
        QTest::addColumn<bool>("fill");
        QTest::addColumn<int>("begin");
        QTest::addColumn<int>("end");

        QTest::newRow("") << 0x00000000u << 32 << false << 0 << 31;
        QTest::newRow("") << 0x00000000u << 32 << true  << 0 << 31;
        QTest::newRow("") << 0x55555555u << 32 << false << 0 << 31;
        QTest::newRow("") << 0x55555555u << 32 << true  << 0 << 31;
        QTest::newRow("") << 0x12345678u << 32 << false << 0 << 31;
        QTest::newRow("") << 0x12345678u << 32 << true  << 0 << 31;
        QTest::newRow("") << 0xFFFFFFFFu << 32 << false << 0 << 31;
        QTest::newRow("") << 0xFFFFFFFFu << 32 << true  << 0 << 31;

        QTest::newRow("") << 0x00000000u << 32 << false << 3 << 27;
        QTest::newRow("") << 0x00000000u << 32 << true  << 3 << 27;
        QTest::newRow("") << 0x55555555u << 32 << false << 3 << 27;
        QTest::newRow("") << 0x55555555u << 32 << true  << 3 << 27;
        QTest::newRow("") << 0x12345678u << 32 << false << 3 << 27;
        QTest::newRow("") << 0x12345678u << 32 << true  << 3 << 27;
        QTest::newRow("") << 0xFFFFFFFFu << 32 << false << 3 << 27;
        QTest::newRow("") << 0xFFFFFFFFu << 32 << true  << 3 << 27;

        QTest::newRow("") << 0x00000000u << 28 << false << 3 << 27;
        QTest::newRow("") << 0x00000000u << 28 << true  << 3 << 27;
        QTest::newRow("") << 0x55555555u << 28 << false << 3 << 27;
        QTest::newRow("") << 0x55555555u << 28 << true  << 3 << 27;
        QTest::newRow("") << 0x12345678u << 28 << false << 3 << 27;
        QTest::newRow("") << 0x12345678u << 28 << true  << 3 << 27;
        QTest::newRow("") << 0xFFFFFFFFu << 28 << false << 3 << 27;
        QTest::newRow("") << 0xFFFFFFFFu << 28 << true  << 3 << 27;
    }

    void tstBitVector::fillBitVector() {
        QFETCH(unsigned int, init);
        QFETCH(BitVector, pattern);
        QFETCH(int, begin);
        QFETCH(int, end);

        //qDebug() << init << pattern.toInt() << pattern.size() << begin << end;

        BitVector vec = BitVector::fromInt(init, 32);
        vec.fill(pattern, begin, end);

        const unsigned int mask = 0xFFFFFFFF;
        unsigned int ref = init;

        end = qMin(end, begin+pattern.size() - 1);

        ref &= ~((mask << begin) & (mask >> (31-end)));
        ref |= (pattern.toInt() << begin) & (mask >> (31-end));

        QCOMPARE(QString::number(vec.toInt(),2).rightJustified(32, '0'),
                 QString::number(ref, -2+5-1).rightJustified(32, '0')); // quirk to allow 4 aligned output
    }
    void tstBitVector::fillBitVector_data() {
        unsigned int pattern[] = {0x00000000,
                                  0xFFFFFFFF,
                                  0x55555555,
                                  0xAAAAAAAA,
                                  0x12345678,
                                  0x7a5b9c10};

        QTest::addColumn<unsigned int>("init");
        QTest::addColumn<BitVector>("pattern");
        QTest::addColumn<int>("begin");
        QTest::addColumn<int>("end");

        const int patterns = (int) (sizeof(pattern)/sizeof(pattern[0]));
        for(int a=0; a < patterns; a++) {
            for(int b=0; b<patterns; b++) {
                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b], 32) << 0 << 31;
                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b], 16) << 0 << 31;
                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b],  8) << 0 << 31;
                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b],  7) << 0 << 31;
                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b],  9) << 0 << 31;

                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b],  7) << 2 << 31;
                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b],  9) << 2 << 31;

                QTest::newRow("") << pattern[a] << BitVector::fromInt(pattern[b],  32) << 2 << 31;
            }
        }

    }

 // Shift
    void tstBitVector::shift(){
        QFETCH(unsigned int, value);
        QFETCH(int, size);
        QFETCH(int, shift);

        const unsigned int mask = 0xFFFFFFFF >> (32 - size);
        unsigned int ref_left = ((value & mask) << qAbs(shift)) & mask,
                ref_right = ((value & mask) >> qAbs(shift)) & mask;

        if (qAbs(shift) > 31) ref_left = ref_right = 0;
        else if (shift < 0) std::swap(ref_left, ref_right);

        //qDebug() << value << size << shift << ref_left << ref_right;

        BitVector vec = BitVector::fromInt(value, size);
        vec <<= shift;
        QCOMPARE(vec.toInt(), ref_left);
        QCOMPARE((BitVector::fromInt(value, size) << shift).toInt(), ref_left);


        vec = BitVector::fromInt(value, size);
        vec >>= shift;
        QCOMPARE(vec.toInt(), ref_right);
        QCOMPARE((BitVector::fromInt(value, size) >> shift).toInt(), ref_right);
    }


    void tstBitVector::shift_data() {
        unsigned int patterns[] = {0x00000000,
                                  0xFFFFFFFF,
                                  0x55555555,
                                  0xAAAAAAAA,
                                  0x12345678,
                                  0x7a5b9c10};

        const int patterns_count = (int) (sizeof(patterns)/sizeof(patterns[0]));

        QTest::addColumn<unsigned int>("value");
        QTest::addColumn<int>("size");
        QTest::addColumn<int>("shift");

        for(int i=0; i<patterns_count; i++) {
            for(int size=32; size > 0; size -= 3) {
                for(int shift=40; shift > -40; shift -= 3) {
                    QTest::newRow("") << patterns[i] << size << shift;
                }
            }
        }
    }

// Slice
    void tstBitVector::slice() {
        QFETCH(unsigned int, value);
        QFETCH(int, size);
        QFETCH(int, begin);
        QFETCH(int, end);

        BitVector src = BitVector::fromInt(value, size);
        BitVector slc = src.slice(begin, end);

        const unsigned int mask = 0xFFFFFFFF;
        unsigned int ref = value & (mask >> (32 - size));
//        qDebug() << QString::number(ref,2).rightJustified(size, '0') << size << begin << end;
        ref >>= begin;
        ref &= mask >> (begin + 32 - end);


        QCOMPARE(QString::number(slc.toInt(),2).rightJustified(slc.size(), '0'),
                 QString::number(ref, -2+5-1).rightJustified(slc.size(), '0')); // quirk to allow 4 aligned output
    }


    void tstBitVector::slice_data() {
        unsigned int patterns[] = {0x00000000,
                                  0xFFFFFFFF,
                                  0x55555555,
                                  0xAAAAAAAA,
                                  0x12345678,
                                  0x7a5b9c10};

        const int patterns_count = (int) (sizeof(patterns)/sizeof(patterns[0]));

        QTest::addColumn<unsigned int>("value");
        QTest::addColumn<int>("size");
        QTest::addColumn<int>("begin");
        QTest::addColumn<int>("end");

        for(int i=0; i < patterns_count; i++) {
            QTest::newRow("") << patterns[i] << 32 << 0 << 31;
            QTest::newRow("") << patterns[i] << 32 << 3 << 31;
            QTest::newRow("") << patterns[i] << 32 << 8 << 31;
            QTest::newRow("") << patterns[i] << 32 << 9 << 31;

            QTest::newRow("") << patterns[i] << 29 << 0 << 28;
            QTest::newRow("") << patterns[i] << 29 << 3 << 28;
            QTest::newRow("") << patterns[i] << 29 << 8 << 28;
            QTest::newRow("") << patterns[i] << 29 << 9 << 28;

            QTest::newRow("") << patterns[i] << 29 << 0 << 26;
            QTest::newRow("") << patterns[i] << 29 << 3 << 26;
            QTest::newRow("") << patterns[i] << 29 << 8 << 26;
            QTest::newRow("") << patterns[i] << 29 << 9 << 26;

            QTest::newRow("") << patterns[i] << 7 << 0 << 6;
            QTest::newRow("") << patterns[i] << 7 << 3 << 6;
            QTest::newRow("") << patterns[i] << 7 << 2 << 5;
        }
    }

// crc
    void tstBitVector::crc() {
        QFETCH(unsigned int, value);
        QFETCH(int, size);
        QFETCH(int, poly);
        QFETCH(int, degree);
        QFETCH(int, inital);

        unsigned int ref=inital;
        for(int i=size-1; i >= 0; i--) {
            ref <<= 1;
            if (bool(value & (1u << i)) != bool(ref & (1u << degree)))
                ref ^= poly;
        }

        ref &= 0xFFFFFFFF >> (32 - degree);
        //qDebug() << ref;

        QCOMPARE((unsigned int)BitVector::fromInt(value, size).crc(poly, degree, inital), ref);
    }

    void tstBitVector::crc_data() {
        QTest::addColumn<unsigned int>("value");
        QTest::addColumn<int>("size");
        QTest::addColumn<int>("poly");
        QTest::addColumn<int>("degree");
        QTest::addColumn<int>("inital");

        unsigned int patterns[] = {0x12345678, 0x00000000, 0xFFFFFFFF, 0x55555555, 0xAAAAAAAA, 0x7a5b9c10};
        const int patterns_count = (int) (sizeof(patterns)/sizeof(patterns[0]));

        int polys[] = {1, 2, 3, 5, 9, 13};
        const int polys_count = (int) (sizeof(polys)/sizeof(polys[0]));

        for(int a=0; a<patterns_count; a++) {
            for(int b=0; b<patterns_count; b++) {
                for(int p=0; p<polys_count; p++) {
                    for(int d=2; d < 5; d++) {
                        int poly = polys[p] & (0xFFFFFFFF >> (31    -d));

                        QString name;
                        QTextStream str(&name);
                        str << patterns[a] << " " << 32 << " " << polys[p] << " " << d << " " <<  patterns[b];
                        QTest::newRow(name.toAscii()) << patterns[a] << 32 << poly << d << (int)patterns[b];
                    }
                }
            }
        }
    }


 QTEST_APPLESS_MAIN(tstBitVector);

#include "tstBitVector.moc"

