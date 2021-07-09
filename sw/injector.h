/*	INJECTOR DEFINITIONS	*/
#include <stdio.h>
#include <stdint.h>

/*** Definitions ***/
#define INJ_BASE_ADDR 0xfc085000

#define INJ_CTRL 	0x00
#define INJ_STATUS  	0x01
#define INJ_FDESC_PTR 	0x02 

/*
 * Descriptor Type Transaction
 */
#define DESC_TYPE_R	0
#define DESC_TYPE_W	1
#define DESC_TYPE_D	2
/*
 * User defined Clock Cycles Intervals between Transactions 
 */
#define DESC_INTRV_0 	0
#define DESC_INTRV_1 	10
#define DESC_INTRV_2 	20
#define DESC_INTRV_3 	50
#define DESC_INTRV_4 	100
#define DESC_INTRV_5 	250
#define DESC_INTRV_6 	500
#define DESC_INTRV_7 	1000

/*
 * TODO Hardware define the Repetition Counts (and specification document)
 * Transaction Repetition Cycles (Up to 128)
 */
#define DESC_COUNT_0	 0
#define DESC_COUNT_1    1
#define DESC_COUNT_2    2
#define DESC_COUNT_3    5
#define DESC_COUNT_4    10
#define DESC_COUNT_5    25
#define DESC_COUNT_6    50
#define DESC_COUNT_7    100

/*
 * Transaction Sizes
 */
#define DESC_SIZE_4	0x00004
#define DESC_SIZE_8	0x00008
#define DESC_SIZE_16	0x00010
#define DESC_SIZE_32	0x00020
#define DESC_SIZE_64	0x00040
#define DESC_SIZE_128	0x00080
#define DESC_SIZE_256	0x00100
#define DESC_SIZE_512	0x00200
#define DESC_SIZE_1K	0x00400
#define DESC_SIZE_2K	0x00800
#define DESC_SIZE_4K	0x01000
#define DESC_SIZE_8K	0x02000
#define DESC_SIZE_16K	0x04000
#define DESC_SIZE_32K	0x08000
#define DESC_SIZE_64K	0x10000
#define DESC_SIZE_128K	0x20000
#define DESC_SIZE_256K	0x40000



/* Structs */

/*** Injector Structs ***/
typedef struct inj_ctrl {
	int int_en;
	int int_err_en;
	int q_mode;
	int rst;
	int en;
} inj_ctrl;


typedef struct inj_params {
	struct inj_ctrl ctrl; 	//Injector Control Register
	unsigned int fdesc_ptr;	//Injector First Descriptor Pointer
	//TODO Injector Status Register
	//TODO Injector Debug Registers
} inj_params;

/*** Descriptor Structs ***/
typedef struct desc_ctrl {
	int en;
        int type;
        int irqe;
        int addr_fix;
        int count;
        int size;
} desc_ctrl;

typedef struct descriptor {
	unsigned int desc_addr;
	struct desc_ctrl ctrl;
	unsigned int next_desc;
	int last_desc;
	unsigned int dest_addr;
	unsigned int src_addr;
	//TODO Descriptor Status Register

} descriptor;


/*** Functions ***/

/*
 * Write register
 */
void inj_write_reg (unsigned int entry, unsigned int value);

/*
 * Read register
 */
unsigned int inj_read_reg (unsigned int entry);

/* 
 * Start Injector with given configuration 
 */
int inj_run ( inj_params *params );

/* 
 * Stop injector execution 
 */
void inj_stop ( void );

/* 
 * Set up descriptor with given configuration 
 */
void setup_descriptor ( descriptor *descriptor );




