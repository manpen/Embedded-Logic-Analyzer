#ifndef SERIALPORTCHOOSER_H
#define SERIALPORTCHOOSER_H

#include <QString>
#include <QDialog>
#include <QThread>
#include <QMutex>
#include <QWaitCondition>

#include "../hardware/LogicAnalyzer.h"

namespace Ui {
    class SerialPortChooser;
}

class SerialPortTester : public QThread {
    Q_OBJECT

public:
    SerialPortTester(QObject* parent);

    virtual ~SerialPortTester();

    void testPort(QString const & port);

protected:
    void run();

signals:
    void onError(QString const & message);
    void onSuccess(LogicAnalyzerConfiguration const & config);

private:
    QMutex mutex;
    QWaitCondition condition;

    bool restart, abort;
    QString port;
};

class SerialPortChooser : public QDialog
{
    Q_OBJECT

public slots:
    void portChanged();

    void onTesterError(QString const & message);
    void onTesterSuccess(LogicAnalyzerConfiguration const & config);

public:
    explicit SerialPortChooser(QWidget *parent = 0,
                               QString const & defaultPort = QString());
    ~SerialPortChooser();

    QString port();

private:
    Ui::SerialPortChooser *ui;
    SerialPortTester tester;

    bool guiReady;

};

#endif // SERIALPORTCHOOSER_H
