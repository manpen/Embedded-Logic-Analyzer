#ifndef DATATRACE_H
#define DATATRACE_H

#include <vector>
#include <QString>
#include "BitVector.h"

/** @brief Data Frame combines data with validity meta-information */
struct DataFrame {
public:
    /** @brief Default constructor to create @ref invalid=True data frame */
    DataFrame();

    /** @brief Constructor for a valid data frame
      @param data Value of @ref data attribute
      @param duration Value of @ref duration attribute */
    DataFrame(BitVector const & data, int duration = 1);

    /** @brief Indicator whether this frame contains valid data

      If true data should be interpreted as invalid. Typically data contains
      an empty BitVector */
    bool invalid;

    /** Number of ticks this frame covers */
    unsigned int duration;

    /** Data assigned to this frame */
    BitVector data;

    /** Compare operator. Two Frames are considered equal if either
      they are both invalid or if their values match */
    bool operator==(DataFrame const & other) const;
};


class DataTrace : public std::vector<DataFrame> {
public:
    /** initializes an empty data trace */
    DataTrace();

    /** copy constructor */
    DataTrace(DataTrace const & copy);

    /** destructor */
    virtual ~DataTrace();

    /** Get Frame that represents the requested time tick
      @param tick Time Tick
      @return NULL if no frame is found. Otherwise a pointer to this frame */
    const DataFrame* atTime(unsigned int tick) const;
};

#endif // DATATRACE_H
