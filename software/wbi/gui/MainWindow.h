#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <vector>

#include <QMainWindow>
#include <QTimer>
#include <QSettings>
#include <QCheckBox>
#include <QActionGroup>

#include "hardware/LogicAnalyzer.h"

#include "gui/TriggerButtons.h"


namespace Ui {
    class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent, LogicAnalyzer *la);
    ~MainWindow();

public slots:
// Handling
    void show();
    void close();

// Menu
    void showConfig();
    void sendSoftReset();
    void sendColdReset();

// LA settings
    void refreshLAStatus();
    void tpgChanged();

// VCD
    void vcdFileChooser();
    void vcdDownloadFile();

// trigger
    void triggerEnabledChange();
    void triggerSetupChange();
    void triggerTimingChanged();

// Clock Divider
    void systemClockChanged();
    void clockDividerChanged();
    void clockDividerFinished();


private:
    Ui::MainWindow *ui;
    LogicAnalyzer *la;

    QTimer _refreshTimer;
    QSettings _settings;

    /** Test Pattern Generator's Menu ActionGroup used for exclusive selection */
    QActionGroup* _tpgActionGroup;

    /** Trigger Timing Action Group */
    QActionGroup* _ttActionGroup;

    /** Enable checkbox above trigger matrix */
    std::vector<QCheckBox*> _triggerCheckboxes;

    /** Trigger Setup Bit Matrix */
    std::vector<TriggerButtons*> _triggerButtons;


    /** @brief If true gui event handlers dont update hardware
        E.g. used for speeding up the reset of the gui */
    bool _disableHardwareUpdates;

    /** @brief Create trigger matrix */
    void _setupTriggerUI();

    unsigned int _systemClock;
    unsigned int _clockDivider;

    void _updateTimebase(bool updateControls = false);
    QString _normalizeTime(double seconds);
    QString _normalizeFrequency(double frq);

};

#endif // MAINWINDOW_H
