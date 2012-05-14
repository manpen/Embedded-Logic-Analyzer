#include "MainWindow.h"
#include "ui_MainWindow.h"

#include <QVBoxLayout>
#include <QLabel>
#include <QWidget>

#include <QMessageBox>
#include <QFileDialog>

#include <QIntValidator>

#include "math.h"

#include "hardware/VCDExport.h"

MainWindow::MainWindow(QWidget *parent, LogicAnalyzer *la) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    _refreshTimer(this),
    _settings(),
    _triggerCheckboxes(),
    _triggerButtons()
{
    this->la = la;
    _disableHardwareUpdates = true;

    _systemClock = _settings.value("systemClock", 50000000).toUInt();
    _clockDivider = qMax((unsigned int)1, _settings.value("clockDivider", 1).toUInt());

// UI
    ui->setupUi(this);
    layout()->setSizeConstraint( QLayout::SetFixedSize );


    QObject::connect(&_refreshTimer, SIGNAL(timeout()),
                     this, SLOT(refreshLAStatus()));

    ui->lineEditVCD->setText(
        _settings.value("mainwindow/vcdexport", "export.vcd").toString());

    ui->groupBoxVCD->layout()->setAlignment(
                ui->buttonDownload, Qt::AlignRight);

 /* Timebase */
    ui->comboSystemClockUnit->addItem("Hz");
    ui->comboSystemClockUnit->addItem("KHz");
    ui->comboSystemClockUnit->addItem("MHz");
    ui->lineEditSystemClock->setValidator(new QIntValidator(this));

    if (la->configuration().clockDividerWidth) {
        ui->sliderClockDivider->setMaximum(la->configuration().clockDividerWidth * 2);
    } else {
        ui->sliderClockDivider->setEnabled(false);
    }

     _clockDivider = qMin(_clockDivider, 1u << la->configuration().clockDividerWidth);

    _updateTimebase(true);

    _tpgActionGroup = new QActionGroup(this);
    _tpgActionGroup->addAction(ui->menuTPG_Off);
    _tpgActionGroup->addAction(ui->menuTPG_Pattern1);
    _tpgActionGroup->addAction(ui->menuTPG_Pattern2);
    _tpgActionGroup->addAction(ui->menuTPG_Pattern3);
    connect(_tpgActionGroup, SIGNAL(selected(QAction*)),
            this, SLOT(tpgChanged()));

    _ttActionGroup = new QActionGroup(this);
    _ttActionGroup->addAction(ui->menuTT_Pre);
    _ttActionGroup->addAction(ui->menuTT_Center);
    _ttActionGroup->addAction(ui->menuTT_Post);
    connect(_ttActionGroup, SIGNAL(selected(QAction*)),
            this, SLOT(triggerTimingChanged()));

    _setupTriggerUI();
    _disableHardwareUpdates = false;

    sendColdReset();
}

MainWindow::~MainWindow()
{
    _settings.setValue("mainwindow/vcdexport", ui->lineEditVCD->text());
    delete ui;
}

void MainWindow::show() {
    QMainWindow::show();
    refreshLAStatus();
}

void MainWindow::close() {
    QMainWindow::close();
    _refreshTimer.stop();
}

void MainWindow::showConfig() {
    QMessageBox box(this);
    box.addButton(QMessageBox::Ok);

    QString text = "<b>" + tr("Configuration read from client:") + "</b><br /><br />";
    text += la->configuration().toString().replace("\n", "<br />");

    box.setIcon(QMessageBox::Information);
    box.setText(text);
    box.setWindowTitle(tr("Configuration of Logic Analyzer"));
    box.exec();
}

void MainWindow::sendSoftReset() {
    la->resetDevice(false);
    refreshLAStatus();
}

void MainWindow::sendColdReset() {
    _disableHardwareUpdates = true;

    if (la->configuration().supportTestPattern) {
        ui->menuTPG_Off->setChecked(true);
    } else {
        ui->menuTPG->setEnabled(false);
        ui->menuTPG->setStatusTip(
            tr("Test pattern generation not supported by hardware"));
    }

    for(int trg = _triggerCheckboxes.size()-1; trg >= 0; trg--) {
        _triggerCheckboxes[trg]->setChecked(false);
        for(int bits=la->configuration().triggerInputWidth-1; bits >= 0; bits--) {
            _triggerButtons[trg]->setState(bits, 0);
        }
    }

    la->resetDevice(true);

    ui->menuTT_Center->setChecked(true);

    _disableHardwareUpdates = false;

    triggerTimingChanged();
    clockDividerFinished();
}

void MainWindow::refreshLAStatus() {
    static int counter = 0;
    counter = (counter + 1) % 3;

    _refreshTimer.stop();

    LogicAnalyzerSamplingStatus ss = la->readSamplingStatus();
    switch(ss) {
        case EMPTY:
            ui->labelMemoryStatusValue->setText(tr("Empty (untriggered)"));
            ui->buttonDownload->setEnabled(false);
            break;

        case CAPTURING:
            ui->labelMemoryStatusValue->setText(tr("Capturing (triggered)"));
            ui->buttonDownload->setEnabled(false);
            break;

        case FULL:
            ui->labelMemoryStatusValue->setText(tr("Full (triggered)"));
            ui->buttonDownload->setEnabled(true);
            break;
    }

    if (la->configuration().supportAsynchronousReads) {
        ui->labelAsynchValue->setText("0x" + la->readAsynchronousData().toString());
    } else {
        ui->labelAsynchValue->setText(tr("Not supported by hardware"));
    }

    ui->groupStatus->setTitle(tr("Status ") +
                              (counter == 2 ? "..." : (counter ? ".." : ".") ));

    _refreshTimer.start(200);
}

void MainWindow::tpgChanged() {
    if (_disableHardwareUpdates) return;

    for(int i=_tpgActionGroup->actions().size()-1; i>=0; i--) {
        if(reinterpret_cast<QAction*>(_tpgActionGroup->actions()[i])->isChecked()) {
            la->writeTestPatternSetup(i);
            break;
        }
    }
}

void MainWindow::vcdFileChooser() {
    QFileDialog diag(this, Qt::Dialog);
    diag.setWindowTitle(tr("Select VCD-File"));
    diag.setAcceptMode(QFileDialog::AcceptSave);
    diag.setNameFilter(tr("VCD File (*.vcd)"));
    diag.setConfirmOverwrite(true);
    if (diag.exec())
        ui->lineEditVCD->setText(diag.selectedFiles().front());
}

void MainWindow::vcdDownloadFile() {
  // Try to open file
    QString fileName = ui->lineEditVCD->text().trimmed();
    if (fileName.isEmpty()) {
        QMessageBox(QMessageBox::Critical, tr("File Access"),
                    tr("Please fill the location for the VCD file"),
                    QMessageBox::Ok, this).exec();

        ui->lineEditVCD->setFocus();
        return;
    }

    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly)) {
        QMessageBox(QMessageBox::Critical, tr("File Access"),
                    tr("Could not open VCD file:\n") + fileName,
                    QMessageBox::Ok, this).exec();

        ui->lineEditVCD->setFocus();
        return;
    }

    setEnabled(false);

  // Download
    try {
        std::vector<DataTrace> data;
        try {
            data = la->decodeData();
        } catch(...) {
            data.clear();
        }

      // Export
        if (data.empty()) {
            QMessageBox(QMessageBox::Critical, tr("Download"),
                        tr("An unknown error occured while download sampling data"),
                        QMessageBox::Ok, this).exec();
        } else {
            VCDExport vcd;
            vcd.setTimescale( QString("%1ns").arg( double(_clockDivider) / double(_systemClock) * 1e9 , 0, 'f', 0) );
            QTextStream ts(&file);
            vcd.generateVCD(ts, data);
        }

        file.close(); // automatically done by destructor, anyway ..
    } catch(...) {}

    setEnabled(true);
}

// Trigger
void MainWindow::triggerTimingChanged() {
    if (_disableHardwareUpdates) return;

    if (ui->menuTT_Pre->isChecked()) {
        la->writeMemoryBaseOffset(1);
    } else if (ui->menuTT_Center->isChecked()) {
        la->writeMemoryBaseOffset(la->configuration().memoryMaxAddress  / 2);
    } else {
        la->writeMemoryBaseOffset(la->configuration().memoryMaxAddress-1);
    }
}

void MainWindow::_setupTriggerUI() {
    ui->groupTrigger->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
    QGridLayout* layout = reinterpret_cast<QGridLayout*>(ui->groupTrigger->layout());

    // Boring stuff (Initialize button states)
    std::vector<ButtonState> edgeStates;
    std::vector<ButtonState> valueStates;
    std::vector<bool> boolValues;

    boolValues.push_back(false); boolValues.push_back(false);
        edgeStates.push_back(ButtonState(tr("ignore"),
            tr("This bit has no effect on the trigger"),
            boolValues));
        valueStates.push_back(ButtonState("ignore",
            tr("This bit has no effect on the trigger"),
            boolValues));

    boolValues.clear();
    boolValues.push_back(false); boolValues.push_back(true);
        edgeStates.push_back(ButtonState(tr("falling"),
            tr("High-To-Low edge is required"),
            boolValues));
        valueStates.push_back(ButtonState("low",
            tr("Low value is required"),
            boolValues));

    boolValues.clear();
    boolValues.push_back(true); boolValues.push_back(true);
        edgeStates.push_back(ButtonState(tr("rising"),
            tr("High-To-Low edge is required"),
            boolValues));
        valueStates.push_back(ButtonState("high",
            tr("High value is required"),
            boolValues));


    // Initialize checkboxes and buttons (but dont show them yet)
    for(int i=0; i < la->configuration().triggerEdgeCount; i++) {
        _triggerCheckboxes.push_back(new QCheckBox(tr("Edge %1").arg(i+1), this));
        _triggerButtons.push_back(new TriggerButtons(edgeStates,
            la->configuration().triggerInputWidth, this));
    }

    for(int i=0; i < la->configuration().triggerValueCount; i++) {
        _triggerCheckboxes.push_back(new QCheckBox(tr("Value %1").arg(i+1), this));
        _triggerButtons.push_back(new TriggerButtons(valueStates,
            la->configuration().triggerInputWidth, this));
    }

    // Common config on checkboxes and buttons
    for(unsigned int i=0; i < _triggerCheckboxes.size(); i++) {
        connect(_triggerCheckboxes[i], SIGNAL(stateChanged(int)),
                this, SLOT(triggerEnabledChange()));

        connect(_triggerButtons[i], SIGNAL(clicked()),
                this, SLOT(triggerSetupChange()));

        layout->addWidget(_triggerCheckboxes[i], 0, i + 1);
        layout->addWidget(_triggerButtons[i], 1, i+1,
            la->configuration().triggerInputWidth, 1);
    }


    // bit column
    for(int i=0; i < la->configuration().triggerInputWidth; i++) {
        QLabel* label = new QLabel(tr("%1. Bit").arg(i+1), this);
        label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
        label->setSizePolicy(QSizePolicy::Preferred,QSizePolicy::Preferred);
        layout->addWidget(label, i+1, 0);
    }
}

void MainWindow::triggerEnabledChange() {
    if (_disableHardwareUpdates) return;

    BitVector mask(_triggerCheckboxes.size());
    for(int i=_triggerCheckboxes.size()-1; i>=0; i--) {
        mask.setBit(i, _triggerCheckboxes[i]->checkState() == Qt::Checked);
    }

    la->writeTriggerEnableMask(mask);
}

void MainWindow::triggerSetupChange() {
    if (_disableHardwareUpdates) return;

    int id=-1;
    for(int i=_triggerButtons.size()-1; i>=0; i--) {
        if(_triggerButtons[i]==sender()) {
            id = i;
            break;
        }
    }
    if (id < 0) return;

    int triggerId = id;

    if (id >= la->configuration().triggerEdgeCount) {
        triggerId -= la->configuration().triggerEdgeCount;
        la->writeTriggerSetup(triggerId, VALUE_VALUE,
                              _triggerButtons[id]->bitVector(0));
        la->writeTriggerSetup(triggerId, VALUE_SENSITIVITY,
                              _triggerButtons[id]->bitVector(1));

    } else {
        la->writeTriggerSetup(triggerId, EDGE_EDGE,
                              _triggerButtons[id]->bitVector(0));
        la->writeTriggerSetup(triggerId, EDGE_SENSITIVITY,
                              _triggerButtons[id]->bitVector(1));
    }
}

void MainWindow::systemClockChanged() {
    if (_disableHardwareUpdates) return;

    int clock = ui->lineEditSystemClock->text().toUInt() * (unsigned int) pow(1000, ui->comboSystemClockUnit->currentIndex());

    if (!clock) return;

    _systemClock = clock;

    _settings.setValue("systemClock", _systemClock);

    _updateTimebase();
}

void MainWindow::clockDividerChanged() {
    if (_disableHardwareUpdates) return;

    _clockDivider = qMax(1u, qMin(1u << la->configuration().clockDividerWidth,
      (unsigned int) round(pow(2.0, double(ui->sliderClockDivider->value()) / 2.0))));

    _settings.setValue("clockDivider", _clockDivider);
    _updateTimebase();
}

void MainWindow::clockDividerFinished() {
    clockDividerChanged();

    la->writeClockDividerThreshold(BitVector::fromInt(_clockDivider - 1));
}

void MainWindow::_updateTimebase(bool updateControls) {
    if (updateControls) {
        // normalize system clock
        unsigned int clock = _systemClock;
        for(int i = 0; i < ui->comboSystemClockUnit->count(); i++) {
            if (clock < 1000 || i == ui->comboSystemClockUnit->count() - 1) {
                ui->comboSystemClockUnit->setCurrentIndex(i);
                ui->lineEditSystemClock->setText( QString("%1").arg(clock));
            }

            clock /= 1000;
        }

        // update divider
        ui->sliderClockDivider->setValue(
            (int) round(log2(double(_clockDivider)) * 2.0)
        );
    }

    ui->labelSamplingFrequency->setText(_normalizeFrequency(double(_systemClock) / double(_clockDivider)));
    ui->labelTimeResolution->setText(_normalizeTime(double(_clockDivider) / double(_systemClock)));
    ui->labelTotalSamplingTime->setText(_normalizeTime(
        double(la->configuration().memoryMaxAddress) / ceil(double(la->configuration().bankWidth) / 8.0) / la->configuration().bankCount
        * double(_clockDivider) / double(_systemClock)));
}

QString MainWindow::_normalizeTime(double seconds) {
    QStringList ql;
    ql.append("s");
    ql.append("ms");
    ql.append("us");
    ql.append("ns");
    ql.append("ps");

    for(int i=0; i < ql.count(); i++) {
        if (seconds >= 1.0 || i+1 == ql.count()) {
            return  QString("%1").arg(seconds, 0, 'f', 1).append(ql.at(i));
        }

        seconds *= 1000.0;
    }
}

QString MainWindow::_normalizeFrequency(double frq) {
    QStringList ql;
    ql.append("Hz");
    ql.append("KHz");
    ql.append("MHz");
    ql.append("GHz");

    for(int i=0; i < ql.count(); i++) {
        if (frq < 1000.0 || i+1 == ql.count()) {
            return QString("%1").arg(frq, 0, 'f', 1).append(ql.at(i));
        }

        frq /= 1000.0;
    }
}

