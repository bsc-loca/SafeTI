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

// Start Injector Execution given its configuration
int inj_run(inj_config *config) {
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

// Setup for the descriptor control word, common to all descriptor types minus SEQ operations
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

// Setup for the descriptor control word specific for SEQ operations
void setup_descriptor_control_seq(desc_ctrl* descriptor) {
  unsigned int desc_word = 0x00000000;
  desc_word = desc_word | ((descriptor  -> last         %      2)      ); // Last descriptor bit
  desc_word = desc_word | ((descriptor  -> type         %     32) <<  1); // Type of descriptor bit
  desc_word = desc_word | ((descriptor  -> irq_compl_en %      2) <<  6); // Enable interrupt at completion
  desc_word = desc_word | ((descriptor  -> count        %     64) <<  7); // Sequential repetition counter, first field
  desc_word = desc_word | (((descriptor -> size - 1)    %  16384) << 13); // Transfer size
  desc_word = desc_word | (((descriptor -> count  % 2048) /   64) << 27); // Sequential repetition counter, second field

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

// Setup for READ_SEQ and WRITE_SEQ descriptors (2 words)
void setup_descriptor_rd_wr_seq(desc_rd_wr* descriptor) {
  setup_descriptor_control_seq(&(descriptor -> ctrl));      // Setup the control word
  inj_write_reg(INJ_PROGRAM_DESC, descriptor -> act_addr);  // Setup the action address
}


//*** Template Program Functions ***/

// inj_program() generates an injection program that accesses SIZE_RD_WR bytes on
// DESC_ATTACK_ADDR and then waits SIZE_DELAY clock cycles in standby.
// If SIZE_RD_WR is higher than the allowed 524288 bytes, the function will use
// multiple descriptors to access the total size requested.
// The access type is configured with DESC_TYPE, being 0=NOP, 1=RD, 2=WR.
// Putting INJ_QUEUE to 1 sets the injector to repeat the program non-stop until
// disabled or reset.
void inj_program( unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int DESC_ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT ) {
  inj_program_seq(DESC_TYPE, INJ_QUEUE, SIZE_RD_WR, DESC_ATTACK_ADDR, SIZE_DELAY, DESC_COUNT, 1);
}

// inj_program_seq() generates an injection program that accesses SIZE_RD_WR bytes on
// DESC_ATTACK_ADDR and then waits SIZE_DELAY clock cycles in standby.
// In addition, this program is repeated SEQ_REP times while increasing the access
// address with the access size per operation block.
// If SIZE_RD_WR is higher than the allowed 524288 bytes, the function will use
// multiple descriptors to access the total size requested.
// The access type is configured with DESC_TYPE, being 0=NOP, 1=RD, 2=WR.
// Putting INJ_QUEUE to 1 sets the injector to repeat the program non-stop until
// disabled or reset.
void inj_program_seq( unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int DESC_ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT, unsigned int SEQ_REP ) {
  unsigned int i = 0;       // Loop var for access/delay operation
  unsigned int j = 0;       // Loop var for multiple acccess/delay operation blocks
  unsigned int INJ_PROG_N_WORDS = 0; // Total 32-bit words used in the injection program
  unsigned int N_TRANS_DESC = 0; // Number of transaction descriptors to program
  unsigned int N_DELAY_DESC = 0; // Number of delay descriptors to program
  inj_config inj_cfg   = inj_config_rst; // Injector configuration
  desc_ctrl  ctrl_word = desc_ctrl_rst;  // Descriptor control word
  desc_delay delay     = desc_delay_rst; // Descriptor for DELAY type
  desc_rd_wr rd_wr     = desc_rd_wr_rst; // Descriptor for READ and WRTIE types

  // Disable and reset previous injection program and SafeTI configuration
  inj_reset();

  // Initialize injector configuration
  inj_cfg.enable            = 1;
  inj_cfg.reset             = 0;
  if(INJ_QUEUE == 1)
    inj_cfg.queue_mode_en   = 1;
  else
    inj_cfg.queue_mode_en   = 0;
  inj_cfg.irq_prog_compl_en = 0;
  inj_cfg.irq_err_core_en   = 0;
  inj_cfg.irq_err_net_en    = 1;
  inj_cfg.freeze_irq_en     = 0;

  // Limit descriptor size to maximum, while incrementing
  // the number of descriptors to be programmed
  switch(DESC_TYPE) {
    case INJ_OP_READ:  // Basic READ and WRITE operations have a 
    case INJ_OP_WRITE: // maximum size programmable of 524288
      while(SIZE_RD_WR > 524288) { // N DESC_TYPE descriptors
        SIZE_RD_WR   = SIZE_RD_WR - 524288;
        N_TRANS_DESC = N_TRANS_DESC + 1;
      }
      if(SIZE_RD_WR != 0)
        N_TRANS_DESC = N_TRANS_DESC + 1;
    case INJ_OP_READ_SEQ:  // SEQuential READ and WRITE operations have 
    case INJ_OP_WRITE_SEQ: // a maximum size programmable of 16384
      while(SIZE_RD_WR > 16384) { // N DESC_TYPE descriptors
        SIZE_RD_WR   = SIZE_RD_WR - 16384;
        N_TRANS_DESC = N_TRANS_DESC + 1;
      }
      if(SIZE_RD_WR != 0)
        N_TRANS_DESC = N_TRANS_DESC + 1; }

  while(SIZE_DELAY > 524288) { // N DELAY descriptors
    SIZE_DELAY   = SIZE_DELAY - 524288;
    N_DELAY_DESC = N_DELAY_DESC + 1;
  }
  if(SIZE_DELAY != 0)
    N_DELAY_DESC = N_DELAY_DESC + 1;

    ///////////////////////////////////////////////////////
   // Descriptor generation of DESC_TYPE type and setup //
  ///////////////////////////////////////////////////////
  for(j = 0; j < SEQ_REP; j++) { // Access & delay descriptor loop for sequential access
  for(i = 0; i < N_TRANS_DESC; i++) { // Access descriptor loop
    ctrl_word.irq_compl_en = 0;     // Disable interrupt on descriptor completion
    ctrl_word.count   = DESC_COUNT; // Number of repetitions of the descriptor
    if(N_DELAY_DESC == 0 && i == N_TRANS_DESC - 1 && j == SEQ_REP - 1) // Last descriptor bit
      ctrl_word.last  = 1;
    else
      ctrl_word.last  = 0;

    // Insert DESC_TYPE descriptors
    switch(DESC_TYPE) {
      case INJ_OP_DELAY:
        ctrl_word.type   = DESC_TYPE;
        ctrl_word.size   = SIZE_RD_WR;
        delay.ctrl       = ctrl_word;
        setup_descriptor_delay(&delay);
        INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 1;
        break;

      case INJ_OP_READ:
      case INJ_OP_WRITE:
        // Increase SIZE to ATTACK_ADDR so it follows up the access of previous descriptor
        if(i != 0 && i != N_TRANS_DESC - 1) { // Don't increase ATTACK_ADDRESS on first transaction descriptor nor last of the block
          if(DESC_ATTACK_ADDR < 0xFFF80000)  // avoid ATTACK_ADDRESS 32-bit overflow
            DESC_ATTACK_ADDR = DESC_ATTACK_ADDR + 524288;
          else
            printf("ATTACK_ADDR 32-bit overflow prevented on injector programming.\n");
        } // No "break" required since the following code must be executed
      case INJ_OP_READ_FIX:
      case INJ_OP_WRITE_FIX:
        ctrl_word.type   = DESC_TYPE;
        if(i == N_TRANS_DESC - 1) // Set the last SIZE batch on last descriptor,
          ctrl_word.size = SIZE_RD_WR;
        else                      // otherwise, set to max size.
          ctrl_word.size = 524288;
        rd_wr.ctrl       = ctrl_word;
        rd_wr.act_addr   = DESC_ATTACK_ADDR;
        setup_descriptor_rd_wr(&rd_wr);
        INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 2;
        // Add to the address the access size left in case there's sequential repetitions
        if(i == N_TRANS_DESC - 1)
          DESC_ATTACK_ADDR = DESC_ATTACK_ADDR + (SIZE_RD_WR % 524288);
        break;

      case INJ_OP_READ_SEQ:
      case INJ_OP_WRITE_SEQ:
        // Increase SIZE to ATTACK_ADDR so it follows up the access of previous descriptor
        if(i != 0 && i != N_TRANS_DESC - 1)  // Don't increase ATTACK_ADDRESS on first transaction descriptor nor last of the block
          if(DESC_ATTACK_ADDR < (0xFFFFFFFF+1-(1+DESC_COUNT)*SIZE_RD_WR))  // avoid ATTACK_ADDRESS 32-bit overflow
            DESC_ATTACK_ADDR = DESC_ATTACK_ADDR + (1+DESC_COUNT)*SIZE_RD_WR;
          else
            printf("ATTACK_ADDR 32-bit overflow prevented on injector programming.\n");
        ctrl_word.type   = DESC_TYPE;
        if(i == N_TRANS_DESC - 1) // Set the last SIZE batch on last descriptor,
          ctrl_word.size = SIZE_RD_WR;
        else                      // otherwise, set to max size.
          ctrl_word.size = 16384;
        rd_wr.ctrl       = ctrl_word;
        rd_wr.act_addr   = DESC_ATTACK_ADDR;
        setup_descriptor_rd_wr_seq(&rd_wr);
        INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 2;
        // Add to the address the access size left in case there's sequential repetitions
        if(i == N_TRANS_DESC - 1)
          DESC_ATTACK_ADDR = DESC_ATTACK_ADDR + DESC_COUNT*(SIZE_RD_WR % 16384);
        break;

      default:
        printf("ERROR: Unsupported descriptor type on injection program.");
        break;
    } } // DESC_TYPE descriptors inserted

    ///////////////////////////////////////////////////
   // Descriptor generation of DELAY type and setup //
  ///////////////////////////////////////////////////
    ctrl_word.type    = INJ_OP_DELAY;
    ctrl_word.irq_compl_en = 0;   // Disable interrupt on descriptor completion

    // Set the COUNT struct variable to setup the required N descriptor repetitions
    // with the maximum size that is capable.
    // The COUNT struct variable is encoded as -1 from total number of executions.
    if(N_DELAY_DESC == 1 && j == SEQ_REP - 1) {
      ctrl_word.size  = SIZE_DELAY;
      ctrl_word.count = 0;
      ctrl_word.last  = 1;
    } else {
      ctrl_word.size = 524288;
      ctrl_word.count = N_DELAY_DESC - 2; //-2 because 1 is what is left on SIZE_DELAY
      ctrl_word.last  = 0;
    }

    // Setup the maximum size DELAY descriptor
    if(N_DELAY_DESC != 0) {
      delay.ctrl      = ctrl_word;
      setup_descriptor_delay(&delay);
      INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 1;
    }

    // Setup what is left of the original SIZE_DELAY DELAY descriptor
    if(SIZE_DELAY > 0) {
      ctrl_word.type  = INJ_OP_DELAY;
      ctrl_word.irq_compl_en = 0;
      ctrl_word.size  = SIZE_DELAY;
      ctrl_word.count = 0;
      if(j == SEQ_REP - 1)
        ctrl_word.last  = 1;
      else
        ctrl_word.last  = 0;
      delay.ctrl      = ctrl_word;
      setup_descriptor_delay(&delay);
      INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 1;
    }
  } // Access & delay descriptor loop for sequential access

  // Debug line to print the memory usage and number of descriptors programmed.
  //printf("The injection program uses %4d 32-bit words; %4d of %s and %4d of DELAY.\n", INJ_PROG_N_WORDS, N_TRANS_DESC, DESC_TYPE, N_DELAY_DESC);

  // Launch the injector with the configuration
  inj_run(&inj_cfg);
}
