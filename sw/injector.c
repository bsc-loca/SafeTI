/* INJECTOR C DRIVER */

#include <stdio.h>
#include <stdint.h>
#include "injector.h"

/* Public Functions */

/*
 *  Function    : program_descriptor
 *  Description : Program a single descriptor on a SafeTI module.
 *                In addition, checks for values to be in-range.
 *  Parameters  :
 *    SEL         : Select SafeTI module allocated in the APB memory space.
 *                  The base addresses are defined in injector.h.
 *    DESC_TYPE   : Descriptor type setting. 0 = DELAY, 1 = READ, 2 = WRITE.
 *    SIZE        : Descriptor "size" field. For DELAY descriptors, it sets
 *                  the number of clock cycles to stay without operation.
 *                  For READ and WRITE descriptors, it sets the number of
 *                  bytes to access during execution of the descriptor.
 *                  Value range between [1 - 524288] bytes.
 *    ATTACK_ADDR : Initial address where to start the access.
 *                  Exclusive to READ and WRITE descriptors.
 *    COUNT       : Number of repetitions to execute the descriptor.
 *                  Value range between [0 - 63] repetitions.
 *    LAST        : Bit flag to indicate last descriptor in the traffic pattern.
 *    INT_EN      : Bit flag to enable APB interruption when the descriptor is
 *                  fully executed.
 *  Return      : None
 */
void program_descriptor( unsigned int SEL, unsigned int DESC_TYPE, unsigned int SIZE, unsigned int ATTACK_ADDR, unsigned int COUNT, unsigned int LAST, unsigned int INT_EN ) {
  desc_ctrl  ctrl_word = desc_ctrl_rst;  // Descriptor control word
  desc_delay delay     = desc_delay_rst; // Descriptor for DELAY type
  desc_rd_wr rd_wr     = desc_rd_wr_rst; // Descriptor for READ and WRTIE types

  // Check values are in-range and prepare descriptor parameters
  switch(DESC_TYPE) {
    case INJ_OP_READ:
    case INJ_OP_WRITE:
    case INJ_OP_READ_FIX:
    case INJ_OP_WRITE_FIX:
      // Setup second word with attack address
      rd_wr.act_addr = ATTACK_ADDR;
      // Then continue with same setup as DELAY
    case INJ_OP_DELAY:
      if(SIZE > 524288)
        printf("\n\nERROR SW SafeTI: Size too big, maximum is 524288.\n");
      else if(COUNT > 63)
        printf("\n\nERROR SW SafeTI: Descriptor repetition count too big, maximum is 63.\n");
      else
        ctrl_word.type  = DESC_TYPE;
        ctrl_word.size  = SIZE;
        ctrl_word.count = COUNT;
        ctrl_word.irq_compl_en = INT_EN;
      break;
    case INJ_OP_READ_SEQ:
    case INJ_OP_WRITE_SEQ:
      // Setup second word with attack address
      rd_wr.act_addr = ATTACK_ADDR;
      if(SIZE > 16384)
        printf("\n\nERROR SW SafeTI: Size too big, maximum is 16384.\n");
      else if(COUNT > 2047)
        printf("\n\nERROR SW SafeTI: Descriptor repetition count too big, maximum is 2047.\n");
      else
        ctrl_word.type  = DESC_TYPE;
        ctrl_word.size  = SIZE;
        ctrl_word.count = COUNT;
        ctrl_word.irq_compl_en = INT_EN;
      break;
    default:;
      printf("\n\nERROR SW SafeTI: Descriptor type not supported.\n");
  }
  // Setup descriptor on SafeTI
  switch(DESC_TYPE) {
    case INJ_OP_READ:
    case INJ_OP_WRITE:
    case INJ_OP_READ_FIX:
    case INJ_OP_WRITE_FIX:
      rd_wr.ctrl = ctrl_word;
      setup_descriptor_rd_wr(&rd_wr, SEL);
      break;
    case INJ_OP_DELAY:
      delay.ctrl = ctrl_word;
      setup_descriptor_delay(&delay, SEL);
      break;
    case INJ_OP_READ_SEQ:
    case INJ_OP_WRITE_SEQ:
      rd_wr.ctrl = ctrl_word;
      setup_descriptor_rd_wr_seq(&rd_wr, SEL);
      break;
    default:;
      printf("\n\nERROR SW SafeTI: Descriptor type not supported.\n");
  }
}

/*
 *  Function    : program_configuration
 *  Description : Program configuration on a SafeTI module.
 *  Parameters  :
 *    SEL         : Select SafeTI module allocated in the APB memory space.
 *                  The base addresses are defined in injector.h.
 *    ENABLE      : Start SafeTI traffic injection execution.
 *    QUEUE_EN    : Bit flag to enable QUEUE mode, where SafeTI will repeat
 *                  programmed traffic pattern from the start when it ends.
 *    INT_PROG_COMPL  : Bit flag to enable APB interruption when the traffic
 *                      pattern has been fully executed.
 *    INT_ERROR   : Bit flag to enable APB interruption when the SafeTI
 *                  encounters an internal error (eg, wrong decode).
 *    INT_NET_ERROR   : Bit flag to enable APB interruption when the SafeTI
 *                      receives an error response from the network.
 *    FREEZE_INT  : Bit flag to stop SafeTI when there's an APB interruption.
 *  Return      : None
 */
void program_configuration( unsigned int SEL, unsigned int ENABLE, unsigned int QUEUE_EN, unsigned int INT_PROG_COMPL, unsigned int INT_ERROR, unsigned int INT_NET_ERROR, unsigned int FREEZE_INT ) {
  inj_config inj_cfg   = inj_config_rst; // Injector configuration initialized
  // Set configuration parameters
  inj_cfg.enable            = ENABLE;
  inj_cfg.reset             = 0;
  inj_cfg.queue_mode_en     = QUEUE_EN;
  inj_cfg.irq_prog_compl_en = INT_PROG_COMPL;
  inj_cfg.irq_err_core_en   = INT_ERROR;
  inj_cfg.irq_err_net_en    = INT_NET_ERROR;
  inj_cfg.freeze_irq_en     = FREEZE_INT;
  // Send configuration to traffic injector (and start if ENABLE = 1)
  inj_run(&inj_cfg, SEL);
}

/*
 *  Function    : inj_reset
 *  Description : Stop and reset internal memory on a SafeTI module.
 *  Parameters  :
 *    SEL         : Select SafeTI module allocated in the APB memory space.
 *                  The base addresses are defined in injector.h.
 *  Return      : None
 */
void inj_reset(unsigned int sel) {
  //Configuration register (0x00) (Reset bit)
  inj_write_reg(INJ_CONFIG, 0x00000002, sel);
}

/*
 *  Function    : inj_check_run
 *  Description : Check if the SafeTI module is in execution.
 *  Parameters  :
 *    SEL         : Select SafeTI module allocated in the APB memory space.
 *                  The base addresses are defined in injector.h.
 *  Return      :
 *    - 1         : The SafeTI module is executing descriptors.
 *    - 0         : The SafeTI module is disabled.
 */
// Check if the injector is running. Returns 1 if is running.
unsigned int inj_check_run(unsigned int sel) {
  unsigned int running = (inj_read_reg(INJ_CONFIG, sel) & 0x00000001);
  return running;
}


/* Private Functions */

// Get SafeTI "sel" base address in the APB memory space
unsigned int inj_get_base_addr(unsigned int sel) {
  switch(sel) {
    case  0: return INJ_0_BASE_ADDR;
    case  1: return INJ_1_BASE_ADDR;
    case  2: return INJ_2_BASE_ADDR;
    case  3: return INJ_3_BASE_ADDR;
    case  4: return INJ_4_BASE_ADDR;
    case  5: return INJ_5_BASE_ADDR;
    case  6: return INJ_6_BASE_ADDR;
    case  7: return INJ_7_BASE_ADDR;
    case  8: return INJ_8_BASE_ADDR;
    case  9: return INJ_9_BASE_ADDR;
    case 10: return INJ_A_BASE_ADDR;
    case 11: return INJ_B_BASE_ADDR;
    case 12: return INJ_C_BASE_ADDR;
    case 13: return INJ_D_BASE_ADDR;
    case 14: return INJ_E_BASE_ADDR;
    case 15: return INJ_F_BASE_ADDR;
    default: printf("\n\nERROR SW SafeTI: Injector APB address not found.\n");
  }
  return 0;
}

// Write Injector APB Register
void inj_write_reg(unsigned int entry, unsigned int value, unsigned int sel) {
  volatile unsigned int *p;
  volatile unsigned int base_address = 0;
  // Get Injector "sel" base APB address
  base_address = inj_get_base_addr(sel);
  if(base_address != 0) {
  // Write data to that address
    if(entry < APB_MEM_SPACE) {
      p = (unsigned int*)(base_address + entry*4);
      *p = value;
    } else
      printf("\n\nERROR SW SafeTI: Software tried to write outside the injector's allocated memory space APB_MEM_SPACE.\n");
  }
}

// Read Injector APB Register
unsigned int inj_read_reg (unsigned int entry, unsigned int sel) {
  volatile unsigned int *p;
  volatile unsigned int value = 0;
  volatile unsigned int base_address = 0;
  // Get Injector "sel" base APB address
  base_address = inj_get_base_addr(sel);
  if(base_address != 0) {
  // Read data to that address
    if(entry < APB_MEM_SPACE) {
      p = (unsigned int*)(base_address + ((entry % APB_MEM_SPACE)*4));
      value = *p;
    } else {
      printf("\n\nERROR SW SafeTI: Software tried to read outside the injector's allocated memory space APB_MEM_SPACE.\n");
      value = 0;
  }  }
  return value;
}

// Start Injector "SELENE" Execution given its configuration
void inj_run(inj_config *config, unsigned int sel) {
  unsigned int inj_reg = 0x00000000;

  inj_reg = inj_reg | ((config -> enable             % 2)     );
  inj_reg = inj_reg | ((config -> reset              % 2) << 1);
  inj_reg = inj_reg | ((config -> queue_mode_en      % 2) << 2);
  inj_reg = inj_reg | ((config -> irq_prog_compl_en  % 2) << 3);
  inj_reg = inj_reg | ((config -> irq_err_core_en    % 2) << 4);
  inj_reg = inj_reg | ((config -> irq_err_net_en     % 2) << 5);
  inj_reg = inj_reg | ((config -> freeze_irq_en      % 2) << 6);
  // Control register
  inj_write_reg(INJ_CONFIG, inj_reg, sel);
}


/*** Functions for setting up descriptors ***/

// Setup for the descriptor control word, common to all descriptor types minus SEQ operations
void setup_descriptor_control(desc_ctrl* descriptor, unsigned int sel) {
  unsigned int desc_word = 0x00000000;
  desc_word = desc_word | ((descriptor  -> last         %      2)      ); // Last descriptor bit
  desc_word = desc_word | ((descriptor  -> type         %     32) <<  1); // Type of descriptor bit
  desc_word = desc_word | ((descriptor  -> irq_compl_en %      2) <<  6); // Enable interrupt at completion
  desc_word = desc_word | ((descriptor  -> count        %     64) <<  7); // Repetition counter
  desc_word = desc_word | (((descriptor -> size - 1)    % 524288) << 13); // Transfer size

  // Write descriptor word to SafeTI APB serial
  inj_write_reg(INJ_PROGRAM_DESC, desc_word, sel);
}

// Setup for the descriptor control word specific for SEQ operations
void setup_descriptor_control_seq(desc_ctrl* descriptor, unsigned int sel) {
  unsigned int desc_word = 0x00000000;
  desc_word = desc_word | ((descriptor  -> last         %      2)      ); // Last descriptor bit
  desc_word = desc_word | ((descriptor  -> type         %     32) <<  1); // Type of descriptor bit
  desc_word = desc_word | ((descriptor  -> irq_compl_en %      2) <<  6); // Enable interrupt at completion
  desc_word = desc_word | ((descriptor  -> count        %     64) <<  7); // Sequential repetition counter, first field
  desc_word = desc_word | (((descriptor -> size - 1)    %  16384) << 13); // Transfer size
  desc_word = desc_word | (((descriptor -> count  % 2048) /   64) << 27); // Sequential repetition counter, second field

  // Write descriptor word to SafeTI APB serial
  inj_write_reg(INJ_PROGRAM_DESC, desc_word, sel);
}

// Setup for DELAY descriptors (1 word)
void setup_descriptor_delay(desc_delay* descriptor, unsigned int sel) {
  setup_descriptor_control(&(descriptor -> ctrl), sel);    // Setup the control word
}

// Setup for READ and WRITE descriptors (2 words)
void setup_descriptor_rd_wr(desc_rd_wr* descriptor, unsigned int sel) {
  setup_descriptor_control(&(descriptor -> ctrl), sel);         // Setup the control word
  inj_write_reg(INJ_PROGRAM_DESC, descriptor -> act_addr, sel); // Setup the action address
}

// Setup for READ_SEQ and WRITE_SEQ descriptors (2 words)
void setup_descriptor_rd_wr_seq(desc_rd_wr* descriptor, unsigned int sel) {
  setup_descriptor_control_seq(&(descriptor -> ctrl), sel);     // Setup the control word
  inj_write_reg(INJ_PROGRAM_DESC, descriptor -> act_addr, sel); // Setup the action address
}


//*** Template Program Functions ***/

// inj_program() generates an injection program that accesses SIZE_RD_WR bytes on
// ATTACK_ADDR and then waits SIZE_DELAY clock cycles in standby.
// If SIZE_RD_WR is higher than the allowed 524288 bytes, the function will use
// multiple descriptors to access the total size requested.
// The access type is configured with DESC_TYPE, being 0=NOP, 1=RD, 2=WR.
// Putting INJ_QUEUE to 1 sets the injector to repeat the program non-stop until
// disabled or reset.
void inj_program( unsigned int SEL, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT ) {
  inj_program_seq(SEL, DESC_TYPE, INJ_QUEUE, SIZE_RD_WR, ATTACK_ADDR, SIZE_DELAY, DESC_COUNT, 1);
}

// inj_program_seq() generates an injection program that accesses SIZE_RD_WR bytes on
// ATTACK_ADDR and then waits SIZE_DELAY clock cycles in standby.
// In addition, this program is repeated SEQ_REP times while increasing the access
// address with the access size per operation block.
// If SIZE_RD_WR is higher than the allowed 524288 bytes, the function will use
// multiple descriptors to access the total size requested.
// The access type is configured with DESC_TYPE, being 0=NOP, 1=RD, 2=WR.
// Putting INJ_QUEUE to 1 sets the injector to repeat the program non-stop until
// disabled or reset.
void inj_program_seq( unsigned int SEL, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT, unsigned int SEQ_REP ) {
  unsigned int i = 0;       // Loop var for access/delay operation
  unsigned int j = 0;       // Loop var for multiple acccess/delay operation blocks
  unsigned int INJ_PROG_N_WORDS = 0; // Total 32-bit words used in the injection program
  unsigned int N_TRANS_DESC = 0; // Number of transaction descriptors to program
  unsigned int N_DELAY_DESC = 0; // Number of delay descriptors to program
  desc_ctrl  ctrl_word = desc_ctrl_rst;  // Descriptor control word
  desc_delay delay     = desc_delay_rst; // Descriptor for DELAY type
  desc_rd_wr rd_wr     = desc_rd_wr_rst; // Descriptor for READ and WRTIE types

  // Disable and reset previous injection program and SafeTI configuration
  inj_reset(SEL);

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
        setup_descriptor_delay(&delay, SEL);
        INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 1;
        break;

      case INJ_OP_READ:
      case INJ_OP_WRITE:
        // Increase SIZE to ATTACK_ADDR so it follows up the access of previous descriptor
        if(i != 0 && i != N_TRANS_DESC - 1) { // Don't increase ATTACK_ADDRESS on first transaction descriptor nor last of the block
          if(ATTACK_ADDR < 0xFFF80000)  // avoid ATTACK_ADDRESS 32-bit overflow
            ATTACK_ADDR = ATTACK_ADDR + 524288;
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
        rd_wr.act_addr   = ATTACK_ADDR;
        setup_descriptor_rd_wr(&rd_wr, SEL);
        INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 2;
        // Add to the address the access size left in case there's sequential repetitions
        if(i == N_TRANS_DESC - 1)
          ATTACK_ADDR = ATTACK_ADDR + (SIZE_RD_WR % 524288);
        break;

      case INJ_OP_READ_SEQ:
      case INJ_OP_WRITE_SEQ:
        // Increase SIZE to ATTACK_ADDR so it follows up the access of previous descriptor
        if(i != 0 && i != N_TRANS_DESC - 1)  // Don't increase ATTACK_ADDRESS on first transaction descriptor nor last of the block
          if(ATTACK_ADDR < (0xFFFFFFFF+1-(1+DESC_COUNT)*SIZE_RD_WR))  // avoid ATTACK_ADDRESS 32-bit overflow
            ATTACK_ADDR = ATTACK_ADDR + (1+DESC_COUNT)*SIZE_RD_WR;
          else
            printf("ATTACK_ADDR 32-bit overflow prevented on injector programming.\n");
        ctrl_word.type   = DESC_TYPE;
        if(i == N_TRANS_DESC - 1) // Set the last SIZE batch on last descriptor,
          ctrl_word.size = SIZE_RD_WR;
        else                      // otherwise, set to max size.
          ctrl_word.size = 16384;
        rd_wr.ctrl       = ctrl_word;
        rd_wr.act_addr   = ATTACK_ADDR;
        setup_descriptor_rd_wr_seq(&rd_wr, SEL);
        INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 2;
        // Add to the address the access size left in case there's sequential repetitions
        if(i == N_TRANS_DESC - 1)
          ATTACK_ADDR = ATTACK_ADDR + DESC_COUNT*(SIZE_RD_WR % 16384);
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
      setup_descriptor_delay(&delay, SEL);
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
      setup_descriptor_delay(&delay, SEL);
      INJ_PROG_N_WORDS = INJ_PROG_N_WORDS + 1;
    }
  } // Access & delay descriptor loop for sequential access

  // Debug line to print the memory usage and number of descriptors programmed.
  //printf("The injection program uses %4d 32-bit words; %4d of %s and %4d of DELAY.\n", INJ_PROG_N_WORDS, N_TRANS_DESC, DESC_TYPE, N_DELAY_DESC);

  // Launch the injector with the configuration
  // (ENABLE, QUEUE_EN, INT_PROG_COMPL, INT_ERROR, INT_NET_ERROR, FREEZE_INT)
  program_configuration(SEL, 1, INJ_QUEUE, 0, 0, 0, 0);
}
