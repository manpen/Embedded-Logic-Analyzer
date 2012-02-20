#ifndef VCDEXPORT_H
#define VCDEXPORT_H

#include <vector>
#include <QString>
#include <QTextStream>
#include "DataTrace.h"

class VCDExport
{
public:
    VCDExport();

    /** Meta-Information @b comment of VCD-Header */
    inline QString const & comment() const {return _comment;}

    /** Meta-Information @b date of VCD-Header */
    inline QString const & date() const {return _date;}

    /** Meta-Information @b timescale of VCD-Header */
    inline QString const & timescale() const {return _timescale;}

    /** Meta-Information @b version of VCD-Header */
    inline QString const & version() const {return _version;}

    /** Meta-Information @b comment of VCD-Header */
    inline void setComment(QString const & v) {_comment = v;}

    /** Meta-Information @b date of VCD-Header */
    inline void setDate(QString const & v) {_date = v;}

    /** Meta-Information @b timescale of VCD-Header
        @param v Timescale in format 0.0us */
    inline void setTimescale(QString const & v) {_timescale = v;}

    /** Meta-Information @b version of VCD-Header */
    inline void setVersion(QString const & v) {_version = v;}

    /** Same as generateVCD but instead of writing to a stream,
        it returns a string. This might increase the memory consumption. */
    QString toString(std::vector<DataTrace> const & data) const;

    /** Generates a complete VCD-File incl. Header and write
      result to a TextStream. */
    void generateVCD(QTextStream & stream,
                     std::vector<DataTrace> const & data) const;

protected:
    QString _comment;
    QString _date;
    QString _timescale;
    QString _version;

    QString _varIdentifier(int v) const;
};

#endif // VCDEXPORT_H
