#ifndef TRIGGERBUTTONS_H
#define TRIGGERBUTTONS_H

#include <vector>

#include <QWidget>
#include <QPushButton>

#include "hardware/BitVector.h"

struct ButtonState {
    ButtonState(QString const & text, QString const & statusTip,
                std::vector<bool> const & values);

    QString text;
    QString statusTip;
    std::vector<bool> values;
};

/** @brief Widget that presents a number of multivalue toggle buttons */
class TriggerButtons : public QWidget
{
    Q_OBJECT
public:
    explicit TriggerButtons(std::vector<ButtonState> const & states,
                            unsigned int bits,
                            QWidget *parent = 0);

    /** @brief Number of the state the bit-th buttons is in */
    inline int state(int bit) const {return _buttonStates[bit];}

    /** @brief BitVector represented by buttons */
    BitVector bitVector(unsigned int vec) const;

signals:
    /** @brief Fired when button changes */
    void clicked();

public slots:
    /** @brief Set state of button
      @param bit Button to alter
      @param state Number of new state */
    void setState(unsigned int bit, unsigned int state);

private slots:
    void _clicked();

protected:
    std::vector<ButtonState> _states;
    std::vector<int> _buttonStates;
    std::vector<QPushButton*> _buttons;
};

#endif // TRIGGERBUTTONS_H
