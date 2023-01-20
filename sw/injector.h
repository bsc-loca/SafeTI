/*  INJECTOR DEFINITIONS  */
#include <stdio.h>
#include <stdint.h>

/*** Definitions that may vary ***/
#define INJ_BASE_ADDR   0xfc095000  // Injector base APB addrees
#define APB_MEM_SPACE   0x40        // Number of 32-bit spaces assigned to injector

//----------------------------------
//   HARDWARE SPECIFIC CONSTANTS (unused at the moment)
//----------------------------------
//#define LINE_SIZE 32
//#define L1_CACHE_SIZE (16*1024)
//#define L1_WAYS 4
//#define L1_WAY_SIZE (L1_CACHE_SIZE/L1_WAYS)

//SELENE
//#define L2_CACHE_SIZE (1024*1024)
//DERISC
//#define L2_CACHE_SIZE (256*1024)

//----------------------------------
//    TEST SPECIFIC CONSTANTS (unused at the moment)
//----------------------------------
//#define ITER_COUNT 64

// In each loop iteration we perform ITER_COUNT ld/st for each cache line
//#define ITERATION_INCREMENT (LINE_SIZE*ITER_COUNT)

// Using this defines we can execute nops between load/stores
//By setting this parameter to 1 a jump (jal) is introduced between each ld/st
//The jump is to a loop that executes nops
//#define NOP_ACTIVATED 0
//This parameter configures the number of nops inside the loop
//#define NOP_COUNT 10
//This parameter configures the number of itereations that the loop is executed
//#define NOP_ITERATIONS 15

  ////////////////////////
 // SafeTI Definitions //
////////////////////////

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

// Write APB Injector register
void inj_write_reg (unsigned int entry, unsigned int value);

// Read APB Injector register
unsigned int inj_read_reg (unsigned int entry);

// Start Injector given its configuration
int inj_run ( inj_config *config );

// Reset injector execution and programming
void inj_reset ( void );

// Check if the injector is running.
unsigned int inj_check_run( void );

// Set up descriptor control word (common)
void setup_descriptor_control(desc_ctrl* descriptor);

// Set up descriptor control word for SEQ operations
void setup_descriptor_control_seq(desc_ctrl* descriptor);

// Set up DELAY descriptor
void setup_descriptor_delay(desc_delay* descriptor);

// Set up READ or WRITE descriptor
void setup_descriptor_rd_wr(desc_rd_wr* descriptor);

// Set up READ_SEQ or WRITE_SEQ descriptor
void setup_descriptor_rd_wr_seq(desc_rd_wr* descriptor);

// Template programs for the injector (write on SafeTI APB serial)
void inj_program( unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int DESC_ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT );

void inj_program_seq( unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int DESC_ATTACK_ADDR, unsigned int SIZE_DELAY, unsigned int DESC_COUNT, unsigned int SEQ_REP );
