#include "TriggerButtons.h"

#include <QVBoxLayout>

ButtonState::ButtonState(QString const & text, QString const & statusTip,
                         std::vector<bool> const & values)
    : text(text), statusTip(statusTip), values(values) {}

TriggerButtons::TriggerButtons(const std::vector<ButtonState> &states,
                               unsigned int bits,
                               QWidget *parent) :
    QWidget(parent), _states(states), _buttonStates(bits, 0), _buttons()
{
 // check parameters
    Q_ASSERT(bits > 0);
    Q_ASSERT(states.size() > 0);

    unsigned int size = states.at(0).values.size();
    for(unsigned int i=1; i < states.size(); i++)
        Q_ASSERT(states.at(i).values.size() == size);

 // create gui
    setLayout(new QVBoxLayout());
    for(unsigned int i=0; i < bits; i++) {
        QPushButton* button = new QPushButton(this);
        _buttons.push_back(button);
        setState(i, 0);
        layout()->addWidget(button);
        connect(button, SIGNAL(clicked()), this, SLOT(_clicked()));
    }

    layout()->setMargin(0);
}

BitVector TriggerButtons::bitVector(unsigned int vec) const {
    Q_ASSERT(_states[0].values.size() > vec);

    BitVector result(_buttonStates.size());
    for(unsigned int i=0; i < _buttons.size(); i++)
        result.setBit(i, _states[_buttonStates[i]].values[vec]);

    return result;
}

void TriggerButtons::setState(unsigned int bit, unsigned int state) {
    Q_ASSERT(bit < _buttons.size());
    Q_ASSERT(state < _states.size());

    _buttonStates[bit] = state;
    _buttons[bit]->setText(_states[state].text);
    _buttons[bit]->setStatusTip(_states[state].statusTip);

    if (state) {
        _buttons[bit]->setStyleSheet("* {font: 750}");
    } else {
        _buttons[bit]->setStyleSheet("");
    }
}

void TriggerButtons::_clicked() {
    QPushButton *button = reinterpret_cast<QPushButton*>(sender());

 // search button
    int id = -1;
    for(unsigned int i=0; i<_buttons.size(); i++) {
        if (button == _buttons[i]) {
            id = i;
            break;
        }
    }

    Q_ASSERT(id >= 0);

 // update state
    int newState = (_buttonStates[id] + 1) % _states.size();
    setState(id, newState);

    emit clicked();
}
