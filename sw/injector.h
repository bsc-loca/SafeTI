/*  INJECTOR DEFINITIONS  */
#include <stdio.h>
#include <stdint.h>

  ////////////////////////
 // SafeTI HW Pointers //
////////////////////////

// Base addressess for SafeTI modules and label. The address must match
// with where a SafeTI module is allocated in the HW design.
#define SAFETI_0_BASE_ADDR 0xfc085000 // SafeTI  0 base address
#define SAFETI_AXI_0 0                // SafeTI  0 label
#define SAFETI_1_BASE_ADDR 0xfc095000 // SafeTI  1 base address
#define SAFETI_AHB_0 1                // SafeTI  1 label
#define SAFETI_2_BASE_ADDR 0xfc095100 // SafeTI  2 base address
#define SAFETI_AHB_1 2                // SafeTI  2 label
#define SAFETI_3_BASE_ADDR 0xfc095000 // SafeTI  3 base address
//#define SAFETI_AHB_0 3                // SafeTI  3 label
#define SAFETI_4_BASE_ADDR 0xfc095000 // SafeTI  4 base address
//#define SAFETI_AHB_0 4                // SafeTI  4 label
#define SAFETI_5_BASE_ADDR 0xfc095000 // SafeTI  5 base address
//#define SAFETI_AHB_0 5                // SafeTI  5 label
#define SAFETI_6_BASE_ADDR 0xfc095000 // SafeTI  6 base address
//#define SAFETI_AHB_0 6                // SafeTI  6 label
#define SAFETI_7_BASE_ADDR 0xfc095000 // SafeTI  7 base address
//#define SAFETI_AHB_0 7                // SafeTI  7 label
#define SAFETI_8_BASE_ADDR 0xfc095000 // SafeTI  8 base address
//#define SAFETI_AHB_0 8                // SafeTI  8 label
#define SAFETI_9_BASE_ADDR 0xfc095000 // SafeTI  9 base address
//#define SAFETI_AHB_0 9                // SafeTI  9 label
#define SAFETI_A_BASE_ADDR 0xfc095000 // SafeTI 10 base address
//#define SAFETI_AHB_0 10               // SafeTI 10 label
#define SAFETI_B_BASE_ADDR 0xfc095000 // SafeTI 11 base address
//#define SAFETI_AHB_0 11               // SafeTI 11 label
#define SAFETI_C_BASE_ADDR 0xfc095000 // SafeTI 12 base address
//#define SAFETI_AHB_0 12               // SafeTI 12 label
#define SAFETI_D_BASE_ADDR 0xfc095000 // SafeTI 13 base address
//#define SAFETI_AHB_0 13               // SafeTI 13 label
#define SAFETI_E_BASE_ADDR 0xfc095000 // SafeTI 14 base address
//#define SAFETI_AHB_0 14               // SafeTI 14 label
#define SAFETI_F_BASE_ADDR 0xfc095000 // SafeTI 15 base address
//#define SAFETI_AHB_0 15               // SafeTI 15 label

// Number bytes allocated for a SafeTI module
#define SAFETI_MEM_SPACE     0x100


  ///////////////////
 // SafeTI Labels //
///////////////////

// Descriptor type encoding labels for DESC_TYPE
#define INJ_OP_DELAY          0
#define INJ_OP_READ           1
#define INJ_OP_WRITE          2
#define INJ_OP_READ_FIX       5
#define INJ_OP_WRITE_FIX      6
#define INJ_OP_OBSERVER_ADDR  16
#define INJ_OP_OBSERVER_HOLD  17

// Counter encoding for labels SEL_COUNTER
#define INJ_CNT_INT       0 // Interrutpion counter
#define INJ_CNT_ACCESS    1 // Requests granted by traffic interface


  ////////////////////////
 // SafeTI Definitions //
////////////////////////

// CSR register offsets
#define CSR_OFFSET_CONFIG       0x00  // R/W
#define CSR_OFFSET_DEBUG_PC     0x60  // R
#define CSR_OFFSET_CNT_INT      0x80  // R/W
#define CSR_OFFSET_CNT_ACCESS   0x84  // R/W
#define CSR_WRITE_DATA          0xF0  // R/W
#define CSR_OFFSET_NET_PROFILE  0xF8  //   W
#define CSR_OFFSET_DESC_W_INPUT 0xFC  //   W

// Stringification for the Macros
#define STR1(x) #x
#define STR(x) STR1(x)


/* Structs */

/*** Injector Structs ***/
typedef struct inj_config {
  int enable;             // [0 - 1]
  int hold;               // [0 - 1]
  int reset;              // [0 - 1]
  int queue_mode_en;      // [0 - 1]
  int irq_prog_compl_en;  // [0 - 1]
  int irq_err_core_en;    // [0 - 1]
  int irq_err_net_en;     // [0 - 1]
  int freeze_irq_en;      // [0 - 1]
} inj_config;


/*** Descriptor Structs ***/
// Common control word
typedef struct desc_ctrl {
  int last;               // [0 -  1]
  int type;               // [0 - 31]
  int irq_compl_en;       // [0 -  1]
  int count;              // [0 - 63]
  int size;               // [1 - 524288]
} desc_ctrl;

// READ, WRITE, OBSERVER_ADDR descriptor struct
typedef struct desc_2word {
  struct desc_ctrl ctrl;  // Word 0
  unsigned int act_addr;  // Word 1 [0 - 0xFFFF_FFFF]
} desc_2word;

// DELAY, OBSERVER_HOLD descriptor struct
typedef struct desc_1word {
  struct desc_ctrl ctrl;  // Word 0
} desc_1word;

/*** Default type structs ***/
static const inj_config inj_config_rst = {
  .enable            = 0,
  .reset             = 0,
  .queue_mode_en     = 0,
  .irq_prog_compl_en = 0,
  .irq_err_core_en   = 0,
  .irq_err_net_en    = 0,
  .freeze_irq_en     = 0
};

static const desc_ctrl desc_ctrl_rst = {
  .last              = 0,
  .type              = 0,
  .irq_compl_en      = 0,
  .count             = 0,
  .size              = 1
};

static const desc_2word desc_2word_rst = {
  .ctrl              = desc_ctrl_rst,
  .act_addr          = 0
};

static const desc_1word desc_1word_rst = {
  .ctrl              = desc_ctrl_rst
};


/*** Public Function Declarations ***/

// Individual descriptor programmer + parameter limit check
unsigned int program_descriptor( unsigned int SEL, unsigned int DESC_TYPE, unsigned int SIZE, unsigned int ATTACK_ADDR, unsigned int COUNT, unsigned int LAST, unsigned int INT_EN );

// Program traffic injector SEL with custom configuration
void program_configuration( unsigned int SEL, unsigned int ENABLE, unsigned int HOLD, unsigned int QUEUE_EN, unsigned int INT_PROG_COMPL, unsigned int INT_ERROR, unsigned int INT_NET_ERROR, unsigned int FREEZE_INT );

// Reset injector execution and programming
void inj_reset ( unsigned int sel );

// Reset injector counters
void inj_reset_counters( unsigned int sel );

// Read counter
unsigned int inj_read_counter( unsigned int sel, unsigned int SEL_COUNTER );

// Check if the injector is running
unsigned int inj_check_run( unsigned int sel );

// Set write data
void inj_write_data( unsigned int sel, unsigned int WR_DATA );


/*** Private Function Declarations ***/

// Get SafeTI "sel" base address in the memory space
unsigned int inj_get_base_addr(unsigned int sel);

// Write CSR Injector register
void inj_write_reg (unsigned int entry, unsigned int value, unsigned int sel);

// Read CSR Injector register
unsigned int inj_read_reg (unsigned int entry, unsigned int sel);

// Start Injector given its configuration
void inj_run ( inj_config *config, unsigned int sel );

// Set up descriptor control word (common)
void setup_descriptor_control(desc_ctrl* descriptor, unsigned int sel);

// Template programs for the injector SEL (write on SafeTI CSR serial)
void inj_program( unsigned int SEL, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT );

void inj_program_seq( unsigned int SEL, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT, unsigned int SEQ_REP );
