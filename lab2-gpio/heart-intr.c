// lab2-gpio/heart-intr.c
// Copyright (c) 2018 J. M. Spivey

#include "hardware.h"

#define TICK 5                // One timer interrupt per 5 millisec

static const unsigned heart[] = {
    0x28f0, 0x5e00, 0x8060
};

static unsigned row = 0;

void advance(void) {
    row++;
    if (row == 3) row = 0;

    GPIO.OUT = heart[row];
}

void timer1_handler(void) {
    if (TIMER1.COMPARE[0]) {
        advance();
        TIMER1.COMPARE[0] = 0;
    }
}

void init_timer(void) {
    TIMER1.STOP = 1;
    TIMER1.MODE = TIMER_MODE_Timer;
    TIMER1.BITMODE = TIMER_BITMODE_16Bit;
    TIMER1.PRESCALER = 4;      // 1MHz = 16MHz / 2^4
    TIMER1.CLEAR = 1;
    TIMER1.CC[0] = 1000 * TICK;
    TIMER1.SHORTS = BIT(TIMER_COMPARE0_CLEAR);
    TIMER1.INTENSET = BIT(TIMER_INT_COMPARE0);
    TIMER1.START = 1;
    enable_irq(TIMER1_IRQ);
}

void init(void) {
    GPIO.DIR = 0xfff0;
    GPIO.PINCNF[BUTTON_A] = 0;
    GPIO.PINCNF[BUTTON_B] = 0;
    GPIO.OUT = heart[0];
    
    init_timer();

    while (1) {
        pause();
    }
}
