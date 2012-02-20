#include "DataTrace.h"

DataTrace::DataTrace() : std::vector<DataFrame>() {}
DataTrace::DataTrace(DataTrace const & copy) : std::vector<DataFrame>(copy) {}
DataTrace::~DataTrace() {}

/* TODO: DataTrace::atTime not implemented yet
const DataFrame* DataTrace::atTime(unsigned int tick) const {
    Q_ASSERT_X(false, "DataTrace::atTime", "not implemented");
    return NULL;
}
*/

///////////////////////////////////////////////////////////////////////////////
// CLOCK DIVIDER
///////////////////////////////////////////////////////////////////////////////
DataFrame::DataFrame() {
    data = BitVector::EMPTY;
    duration = 1;
    invalid = true;
}

DataFrame::DataFrame(BitVector const & data, int duration) {
    invalid = false;
    this->data = data;
    this->duration = duration;
}

bool DataFrame::operator==(DataFrame const & other) const {
    if (invalid && other.invalid) return true;
    return data == other.data;
}
