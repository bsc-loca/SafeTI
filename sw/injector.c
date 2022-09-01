/* INJECTOR C DRIVER */

#include <stdio.h>
#include <stdint.h>
#include "injector.h"

/* Public Functions */

// Write Injector APB Register
void inj_write_reg(unsigned int entry, unsigned int value) {
  volatile unsigned int *p;
  p = (unsigned int*)(INJ_BASE_ADDR + ((entry % APB_MEM_SPACE)*4));
  *p = value;
}

// Read Injector APB Register
unsigned int inj_read_reg (unsigned int entry) {
  volatile unsigned int *p;
  volatile unsigned int value;
  p = (unsigned int*)(INJ_BASE_ADDR + ((entry % APB_MEM_SPACE)*4));
  value = *p;

  return value;
}

// Setup Injector configuration
int inj_setup(inj_config *config) {
  unsigned int inj_reg = 0x00000000;
  unsigned int rsp = 0;

  inj_reg = inj_reg | ((config -> enable             % 2)     );
  inj_reg = inj_reg | ((config -> reset              % 2) << 1);
  inj_reg = inj_reg | ((config -> queue_mode_en      % 2) << 2);
  inj_reg = inj_reg | ((config -> irq_prog_compl_en  % 2) << 3);
  inj_reg = inj_reg | ((config -> irq_err_core_en    % 2) << 4);
  inj_reg = inj_reg | ((config -> irq_err_net_en     % 2) << 5);
  inj_reg = inj_reg | ((config -> freeze_irq_en      % 2) << 6);
  // Control register
  inj_write_reg(INJ_CONFIG, inj_reg);

  return rsp;
}

// Reset injector execution and programming
void inj_reset(void) {
  //Configuration register (0x00) (Reset bit)
  inj_write_reg(INJ_CONFIG, 0x00000002);
}

// Check if the injector is running. Returns 1 if is running.
unsigned int inj_check_run(void) {
  unsigned int running = (inj_read_reg(INJ_CONFIG) & 0x00000001);
  return running;
}


/*** Functions for setting up descriptors ***/

// Setup for the descriptor control word, common to all descriptor types
void setup_descriptor_control(desc_ctrl* descriptor) {
  unsigned int desc_word = 0x00000000;
  desc_word = desc_word | ((descriptor  -> last         %      2)      ); // Last descriptor bit
  desc_word = desc_word | ((descriptor  -> type         %     32) <<  1); // Type of descriptor bit
  desc_word = desc_word | ((descriptor  -> irq_compl_en %      2) <<  6); // Enable interrupt at completion
  desc_word = desc_word | ((descriptor  -> count        %     64) <<  7); // Repetition counter
  desc_word = desc_word | (((descriptor -> size - 1)    % 524288) << 13); // Transfer size

  // Write descriptor word to SafeTI APB serial
  inj_write_reg(INJ_PROGRAM_DESC, desc_word);
}

// Setup for DELAY descriptors (1 word)
void setup_descriptor_delay(desc_delay* descriptor) {
  setup_descriptor_control(&(descriptor -> ctrl));    // Setup the control word
}

// Setup for READ and WRITE descriptors (2 words)
void setup_descriptor_rd_wr(desc_rd_wr* descriptor) {
  setup_descriptor_control(&(descriptor -> ctrl));          // Setup the control word
  inj_write_reg(INJ_PROGRAM_DESC, descriptor -> act_addr);  // Setup the action address
}


//*** Template Program Functions ***/

// Number of descriptors        / Type of transactions / Enable Queue mode                      /
// Transfer size per descriptor / R/W address          / Delay weigth after transfer descriptor /
void inj_program( unsigned int DESC_N_BATCH, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int DESC_ATTACK_ADDR, unsigned int SIZE_DELAY ) {
  unsigned int i  = 0;  // Loop variable
  inj_config inj_cfg;   // Injector configuration
  desc_ctrl  ctrl_word; // Descriptor's control word
  desc_delay delay;     // Descriptor for DELAY type
  desc_rd_wr rd_wr;     // Descriptor for READ and WRTIE types

  // Check if the injector is running and reset it if it is.
  if(inj_check_run() != 0) {
    inj_reset();
    printf("WARNING: The injector was running before programming it. Thus, it has been reset.\n");
  }

  // Injector configuration
  inj_cfg.enable            = 1;
  inj_cfg.reset             = 0;
  if(INJ_QUEUE == 1)
    inj_cfg.queue_mode_en   = 1;
  else
    inj_cfg.queue_mode_en   = 0;
  inj_cfg.irq_prog_compl_en = 0;
  inj_cfg.irq_err_core_en   = 1;
  inj_cfg.irq_err_net_en    = 1;
  inj_cfg.freeze_irq_en     = 0;

  // Common descriptor setup
  ctrl_word.irq_compl_en    = 0;

  // Descriptor generation and setup
  for(i = 0; i < DESC_N_BATCH; i++) {

    ctrl_word.irq_compl_en = 0;     // Enable interrupt on descriptor completion
    ctrl_word.count   = 0;          // Number of repetitions of the descriptor

    if(i == DESC_N_BATCH - 1)       // Last descriptor bit
      ctrl_word.last  = 1;
    else
      ctrl_word.last  = 0;

    // Insert DELAY descriptor between two READ/WRITE descriptors if SIZE_DELAY has a value
    if(SIZE_DELAY != 0 & i%2 == 1) {
      ctrl_word.type  = INJ_OP_DELAY;
      ctrl_word.size  = SIZE_DELAY;
      delay.ctrl      = ctrl_word;
      setup_descriptor_delay(&delay);
    } else {
      ctrl_word.type  = DESC_TYPE;
      ctrl_word.size  = SIZE_RD_WR;
      rd_wr.ctrl      = ctrl_word;
      rd_wr.act_addr  = DESC_ATTACK_ADDR;
      setup_descriptor_rd_wr(&rd_wr);
    }

  }

  // Launch the injector with the configuration
  inj_setup(&inj_cfg);
}
