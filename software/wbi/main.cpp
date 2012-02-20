#include <iostream>

#include "hardware/WBISerial.h"
#include "hardware/LogicAnalyzer.h"
#include "hardware/VCDExport.h"

#include <QApplication>
#include <QSettings>
#include <QMessageBox>
#include <QDebug>

#include "gui/SerialPortChooser.h"
#include "gui/MainWindow.h"

#define MAIN_SKIP_PORT_CHOOSER 1


using namespace std;

int main(int argc, char ** argv) {
// Register Application
    QApplication app(argc, argv);

    QCoreApplication::setOrganizationName("Penschuck");
    QCoreApplication::setOrganizationDomain("penschuck.eu");
    QCoreApplication::setApplicationName("LA-RS232 Downloader");

    QSettings settings;

// Choose Port
    QString portName = settings.value("rs232/portname", "").toString();

#ifndef MAIN_SKIP_PORT_CHOOSER
    SerialPortChooser chooser(NULL, portName);
    chooser.exec();

    if (chooser.result() == SerialPortChooser::Rejected) {
        QMessageBox(QMessageBox::Critical, QObject::tr("Serial Port"),
                    QObject::tr("No serial port selected. Quit application"),
                    QMessageBox::Ok).exec();

        return -1;
    }

    portName = chooser.port();
    settings.setValue("rs232/portname", portName);
#endif

// Main stuff
    Q_ASSERT(!portName.isEmpty());
    QextSerialPort serial(portName, QextSerialPort::Polling);

    WBISerial::configurePort(serial);
    if (!serial.open(QextSerialPort::ReadWrite)) {
        QMessageBox(QMessageBox::Critical, QObject::tr("Serial Port"),
                    QObject::tr("Error while opening the serial port"),
                    QMessageBox::Ok).exec();

        return -1;
    }

    WBISerial wbi(&serial);
    wbi.connect();

    LogicAnalyzer la(&wbi);
    la.connect();

    if (la.configuration().interfaceVersion != 2) {
        QMessageBox(QMessageBox::Critical, QObject::tr("Logic Analyzer"),
                    QObject::tr("This tool supports only Interface v2, hardware is at: %1").arg(la.configuration().interfaceVersion),
                    QMessageBox::Ok).exec();

        return -1;
    }

    if (la.configuration().minorHardwareVersion != 2) {
        QMessageBox(QMessageBox::Warning, QObject::tr("Logic Analyzer"),
                    QObject::tr("This tool expects a hardware version of 2.2.\nHardware identified as %1\nThis might cause unexpected behaivor").arg(
                        QString::number(la.configuration().interfaceVersion) + "." +  QString::number(la.configuration().minorHardwareVersion)),
                    QMessageBox::Ok).exec();
    }


    MainWindow main(0, &la);
    main.show();

    int ret = app.exec();

    try {la.disconnect();} catch(...) {}
    try {wbi.disconnect();} catch(...) {}
    try {serial.close();} catch(...) {}

    return ret;
}
