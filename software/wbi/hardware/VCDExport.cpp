#include "VCDExport.h"
#include <queue>
#include <QDateTime>

#include <QApplication>

// private helper class to order
class VCDCompareFrame {
public:
    VCDCompareFrame(int tick, int bank, DataFrame const & frame) :
        _frame(frame), _tick(tick), _bank(bank) {}

    // INVERSE ORDER (early first, if equal lower bank first)
    inline bool operator<(VCDCompareFrame const & other) const {
        return _tick > other._tick || (_tick == other._tick && _bank < other._bank);
    }

    inline DataFrame frame() const {return _frame;}
    inline int tick() const {return _tick;}
    inline int bank() const {return _bank;}

protected:
    DataFrame _frame;
    int _tick;
    int _bank;
};

VCDExport::VCDExport() :
    _timescale()
{
    _date = QDateTime::currentDateTime().toString(Qt::ISODate);
    _version = QCoreApplication::applicationVersion();
    _comment = "Create with " + QCoreApplication::applicationName();
}

QString VCDExport::toString(std::vector<DataTrace> const & data) const {
    QString string;
    QTextStream stream(&string);
    generateVCD(stream, data);
    return string;
}

QString VCDExport::_varIdentifier(int v) const {
    QString result;

    do {
        char tmp = 'a';
        tmp += (v % 26);
        result.prepend(QChar(tmp));
        v /= 26;
    } while(v);

    return result;
}

void VCDExport::generateVCD(QTextStream &stream,
                            const std::vector<DataTrace> &data) const {

// inspect data
    int dataWidth = 0;
    for(unsigned int i=0; i < data.size() && !dataWidth; i++) {
        DataTrace const & current = data.at(i);
        for(unsigned int x=0; x < current.size(); x++) {
            if (!current[x].invalid) {
                dataWidth = current[x].data.size();
                break;
            }
        }
    }

    if (!dataWidth) {
        qWarning() << "Try to generate VCD from empty data";
        return;
    }

// meta header
    if (!version().isEmpty())
        stream << "$version "
               << QString(version()).replace("$end", "end")
               << " $end" << endl;

    if (!date().isEmpty())
        stream << "$date "
               << QString(date()).replace("$end", "end")
               << " $end" << endl;

    if (!timescale().isEmpty())
        stream << "$timescale "
               << QString(timescale()).replace("$end", "end")
               << " $end" << endl;

    if (!comment().isEmpty())
        stream << "$comment "
               << QString(comment()).replace("$end", "end")
               << " $end" << endl;

    stream << endl;

// var header
    for(unsigned int i=0; i < data.size(); i++) {
        stream << "$var wire " << dataWidth
               << " " << _varIdentifier(i)
               << " bank" << i << " $end" << endl;
    }

    stream << "$enddefinition $end" << endl << endl;

// build value queue
    std::priority_queue<VCDCompareFrame> queue;
    for(unsigned int bank=0; bank < data.size(); bank++) {
        const DataTrace & trace = data[bank];

        int time = 0;
        for(unsigned int i=0; i < trace.size(); i++) {
            if(!i || !(trace[i-1] == trace[i]))
                queue.push(VCDCompareFrame(time, bank, trace[i]));

            time += trace[i].duration;
        }

        queue.push(VCDCompareFrame(time, bank, DataFrame()));
    }

// dump vars
    stream << "$dumpvars" << endl;

    int tick = -1;
    while(!queue.empty()) {
        if (tick != queue.top().tick()) {
            tick = queue.top().tick();
            stream << "#" << tick << endl;
        }

        stream << "b"
               << (queue.top().frame().invalid
                    ? "Z"
                    : queue.top().frame().data.toString(2, false))
               << " " << _varIdentifier(queue.top().bank()) << endl;

        queue.pop();
    }
}

