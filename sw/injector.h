/*  INJECTOR DEFINITIONS  */
#include <stdio.h>
#include <stdint.h>

/*** Definitions that may vary ***/
// Base addressess for SafeTI modules. These must match the address where
// a SafeTI module is allocated in the HW design.
#define INJ_0_BASE_ADDR 0xfc085000  // Injector  0 base APB addrees
#define INJ_1_BASE_ADDR 0xfc095000  // Injector  1 base APB addrees
#define INJ_2_BASE_ADDR 0xfc095000  // Injector  2 base APB addrees
#define INJ_3_BASE_ADDR 0xfc095000  // Injector  3 base APB addrees
#define INJ_4_BASE_ADDR 0xfc095000  // Injector  4 base APB addrees
#define INJ_5_BASE_ADDR 0xfc095000  // Injector  5 base APB addrees
#define INJ_6_BASE_ADDR 0xfc095000  // Injector  6 base APB addrees
#define INJ_7_BASE_ADDR 0xfc095000  // Injector  7 base APB addrees
#define INJ_8_BASE_ADDR 0xfc095000  // Injector  8 base APB addrees
#define INJ_9_BASE_ADDR 0xfc095000  // Injector  9 base APB addrees
#define INJ_A_BASE_ADDR 0xfc095000  // Injector 10 base APB addrees
#define INJ_B_BASE_ADDR 0xfc095000  // Injector 11 base APB addrees
#define INJ_C_BASE_ADDR 0xfc095000  // Injector 12 base APB addrees
#define INJ_D_BASE_ADDR 0xfc095000  // Injector 13 base APB addrees
#define INJ_E_BASE_ADDR 0xfc095000  // Injector 14 base APB addrees
#define INJ_F_BASE_ADDR 0xfc095000  // Injector 15 base APB addrees


  ////////////////////////
 // SafeTI Definitions //
////////////////////////

// Number of 32-bit addresses allocated for a SafeTI module
#define APB_MEM_SPACE   0x40

// APB Configuration register index
#define INJ_CONFIG        0x00
// APB Debug register indexes
//#define INJ_STATUS        0x01 TO BE IMPLEMENTED
//#define INJ_FDESC_PTR     0x02 TO BE IMPLEMENTED
//#define INJ_DESC_EXEC     0x04 TO BE IMPLEMENTED
//#define INJ_DESC_STATUS   0x08 TO BE IMPLEMENTED
//#define INJ_DESC_PTR      0x09 TO BE IMPLEMENTED
// APB Descriptor input register index
#define INJ_PROGRAM_DESC  0x3F

// Descriptor type encoding
#define INJ_OP_DELAY      0
#define INJ_OP_READ       1
#define INJ_OP_WRITE      2
#define INJ_OP_READ_FIX   5
#define INJ_OP_WRITE_FIX  6
#define INJ_OP_READ_SEQ   9
#define INJ_OP_WRITE_SEQ  10

// Stringification for the Macros
#define STR1(x) #x
#define STR(x) STR1(x)


/* Structs */

/*** Injector Structs ***/
typedef struct inj_config {
  int enable;             // [0 - 1]
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

// READ and WRITE descriptor struct
typedef struct desc_rd_wr {
  struct desc_ctrl ctrl;  // Word 0
  unsigned int act_addr;  // Word 1 [0 - 0xFFFF_FFFF]
} desc_rd_wr;

// DELAY descriptor struct
typedef struct desc_delay {
  struct desc_ctrl ctrl;  // Word 0
} desc_delay;

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

static const desc_rd_wr desc_rd_wr_rst = {
  .ctrl              = desc_ctrl_rst,
  .act_addr          = 0
};

static const desc_delay desc_delay_rst = {
  .ctrl              = desc_ctrl_rst
};


/*** Public Function Declarations ***/

// Individual descriptor programmer + parameter limit check
void program_descriptor( unsigned int SEL, unsigned int DESC_TYPE, unsigned int SIZE, unsigned int ATTACK_ADDR, unsigned int COUNT, unsigned int LAST, unsigned int INT_EN );

// Program traffic injector SEL with custom configuration
void program_configuration( unsigned int SEL, unsigned int ENABLE, unsigned int QUEUE_EN, unsigned int INT_PROG_COMPL, unsigned int INT_ERROR, unsigned int INT_NET_ERROR, unsigned int FREEZE_INT );

// Reset injector execution and programming
void inj_reset ( unsigned int sel );

// Check if the injector is running.
unsigned int inj_check_run( unsigned int sel );


/*** Private Function Declarations ***/

// Get SafeTI "sel" base address in the APB memory space
unsigned int inj_get_base_addr(unsigned int sel);

// Write APB Injector register
void inj_write_reg (unsigned int entry, unsigned int value, unsigned int sel);

// Read APB Injector register
unsigned int inj_read_reg (unsigned int entry, unsigned int sel);

// Start Injector given its configuration
void inj_run ( inj_config *config, unsigned int sel );

// Set up descriptor control word (common)
void setup_descriptor_control(desc_ctrl* descriptor, unsigned int sel);

// Set up descriptor control word for SEQ operations
void setup_descriptor_control_seq(desc_ctrl* descriptor, unsigned int sel);

// Set up DELAY descriptor
void setup_descriptor_delay(desc_delay* descriptor, unsigned int sel);

// Set up READ or WRITE descriptor
void setup_descriptor_rd_wr(desc_rd_wr* descriptor, unsigned int sel);

// Set up READ_SEQ or WRITE_SEQ descriptor
void setup_descriptor_rd_wr_seq(desc_rd_wr* descriptor, unsigned int sel);

// Template programs for the injector SEL (write on SafeTI APB serial)
void inj_program( unsigned int SEL, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT );

void inj_program_seq( unsigned int SEL, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT, unsigned int SEQ_REP );
