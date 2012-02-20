#include "SerialPortChooser.h"
#include "ui_SerialPortChooser.h"
#include "../qextserial/qextserialenumerator.h"
#include "../qextserial/qextserialport.h"
#include "../hardware/WBISerial.h"
#include "../hardware/LogicAnalyzer.h"

#include <QThread>
#include <QDebug>
#include <QMutex>
#include <QWaitCondition>

#include <QPushButton>


SerialPortTester::SerialPortTester(QObject *parent)
                : QThread(parent)
{
    restart = false;
    abort = false;
}

SerialPortTester::~SerialPortTester() {
    mutex.lock();
        abort = true;
        condition.wakeOne();
    mutex.unlock();

    wait();
}

void SerialPortTester::testPort(QString const & port) {
    QMutexLocker locker(&mutex);

    this->port = port;

    if (!isRunning()) {
        start(LowPriority);
    } else {
        restart = true;
        condition.wakeOne();
    }
}


void SerialPortTester::run() {
    while(!abort) {
        QextSerialPort *sp = NULL;
        WBISerial *wbi = NULL;
        LogicAnalyzer *la = NULL;

        mutex.lock();
            QString localPortName = port;
        mutex.unlock();

        QString errorMsg;
        LogicAnalyzerConfiguration config;
        bool success = false;

        try {
            sp = new QextSerialPort(localPortName, QextSerialPort::Polling);
            WBISerial::configurePort(*sp);

            if (!sp->open(QIODevice::ReadWrite)) {
                errorMsg = "Could not open port";
            } else {
                wbi = new WBISerial(sp);
                wbi->setRetries(0);
                if (!wbi->connect()) {
                    errorMsg = "Error while connecting to WBI";
                } else {
                    try{
                        la = new LogicAnalyzer(wbi);
                        la->connect();

                        config = la->configuration();
                        success = true;

                        la->disconnect();
                        wbi->disconnect();
                        sp->disconnect();
                    } catch(...) {
                        errorMsg = "Error while connecting to Logic Analyzer";
                    }
                }
            }

        } catch(...) {
            errorMsg = "Port error";
        }

        if (la)  {delete la;  la  = NULL;}
        if (wbi) {delete wbi; wbi = NULL;}
        if (sp)  {delete sp;  sp  = NULL;}

        if (success) {emit onSuccess(config);}
        else {emit onError(errorMsg);}

        mutex.lock();
        if (!restart)
            condition.wait(&mutex);
        restart = false;
        mutex.unlock();
    }
}

SerialPortChooser::SerialPortChooser(QWidget *parent,
                                     QString const & defaultPort) :
    QDialog(parent),
    ui(new Ui::SerialPortChooser),
    tester(parent)
{
    guiReady = false;
    ui->setupUi(this);
    ui->progressBar->setVisible(false);
    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);

    QList<QextPortInfo> ports = QextSerialEnumerator::getPorts();
    for(int i = 0; i < ports.size(); i++) {
        if (ports[i].physName.isEmpty()) continue;

#if defined WIN32 || defined __WIN32 || defined __WIN32__
        if (!ports[i].portName.startsWith("COM", Qt::CaseInsensitive)) continue;
        ui->comboPort->addItem(ports[i].portName);
#else
        ui->comboPort->addItem(ports[i].physName);
#endif
        if (ports[i].physName == defaultPort)
            ui->comboPort->setCurrentIndex(i);
    }

    qRegisterMetaType<LogicAnalyzerConfiguration>("LogicAnalyzerConfiguration");

    QObject::connect(&tester, SIGNAL(onError(QString)),
                     this, SLOT(onTesterError(QString)));
    QObject::connect(&tester, SIGNAL(onSuccess(LogicAnalyzerConfiguration)),
                     this, SLOT(onTesterSuccess(LogicAnalyzerConfiguration)));

    if (!defaultPort.isEmpty())
        ui->comboPort->setEditText(defaultPort);

    guiReady = true;

    portChanged();
}

SerialPortChooser::~SerialPortChooser()
{
    delete ui;
}

QString SerialPortChooser::port() {
    return ui->comboPort->currentText();
}

/* Check */
void SerialPortChooser::portChanged() {
    if (!guiReady) return;
    if(ui->comboPort->currentText().isEmpty()) return;

    ui->progressBar->setVisible(true);
    ui->textDeviceConfig->setText("Connecting to Logic Analyzer ...");
    ui->textDeviceConfig->setStyleSheet("");
    tester.testPort(ui->comboPort->currentText());
}

void SerialPortChooser::onTesterError(QString const & message) {
    ui->progressBar->setVisible(false);

    ui->textDeviceConfig->setStyleSheet("QTextEdit {color: red;}");
    ui->textDeviceConfig->setText(message);

    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
}

void SerialPortChooser::onTesterSuccess(LogicAnalyzerConfiguration const & config) {
    ui->textDeviceConfig->setStyleSheet("");
    ui->progressBar->setVisible(false);
    ui->textDeviceConfig->setText(config.toString());
    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(true);
}
