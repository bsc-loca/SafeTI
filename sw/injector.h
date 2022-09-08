/*  INJECTOR DEFINITIONS  */
#include <stdio.h>
#include <stdint.h>

/*** Definitions that may require update ***/
#define INJ_BASE_ADDR   0xfc085000  // Injector base APB addrees
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
#define INJ_POINTER_CONFIG  0x00
// APB Debug register indexes
//#define INJ_STATUS        0x01 TO BE IMPLEMENTED
//#define INJ_FDESC_PTR     0x02 TO BE IMPLEMENTED
//#define INJ_DESC_EXEC     0x04 TO BE IMPLEMENTED
//#define INJ_DESC_STATUS   0x08 TO BE IMPLEMENTED
//#define INJ_DESC_PTR      0x09 TO BE IMPLEMENTED
// APB Descriptor input register index
#define INJ_POINTER_PROGRAM 0x3F

// Descriptor type encoding
#define INJ_OP_DELAY      0
#define INJ_OP_READ       1
#define INJ_OP_WRITE      2
#define INJ_OP_READ_FIX   5
#define INJ_OP_WRITE_FIX  6


// Stringification for the Macros
#define STR1(x) #x
#define STR(x) STR1(x)


/* Structs */

/*** Injector Structs ***/
typedef struct inj_config {
  int enable;
  int reset;
  int queue_mode_en;
  int irq_prog_compl_en;
  int irq_err_core_en;
  int irq_err_net_en;
  int freeze_irq_en;
} inj_config;


/*** Descriptor Structs ***/
// Common control word
typedef struct desc_ctrl {
  int last;
  int type;
  int irq_compl_en;
  int count;
  int size;
} desc_ctrl;

// READ and WRITE descriptor struct
typedef struct desc_rd_wr {
  struct desc_ctrl ctrl;  // Word 0
  unsigned int act_addr;  // Word 1
} desc_rd_wr;

// DELAY descriptor struct
typedef struct desc_delay {
  struct desc_ctrl ctrl;  // Word 0
} desc_delay;


/*** Public Function Declarations ***/

// Write APB Injector register
void inj_write_reg(unsigned int entry, unsigned int value);

// Read APB Injector register
unsigned int inj_read_reg(unsigned int entry);

// Setup Injector configuration
int inj_setup( inj_config *config );

// Reset injector execution and programming
void inj_reset( void );

// Check if the injector is running.
unsigned int inj_check_run( void );

// Set up descriptor control word (common)
void setup_descriptor_control(desc_ctrl* descriptor);

// Set up DELAY descriptor
void setup_descriptor_delay(desc_delay* descriptor);

// Set up READ or WRITE descriptor
void setup_descriptor_rd_wr(desc_rd_wr* descriptor);

// Template program for the injector (write on SafeTI APB serial)
void inj_program( unsigned int DESC_N_BATCH, unsigned int DESC_TYPE, unsigned int INJ_QUEUE, unsigned int SIZE_RD_WR, unsigned int DESC_ATTACK_ADDR, unsigned int SIZE_DELAY );
