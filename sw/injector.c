/* INJECTOR C DRIVER */ 

#include "injector.h"
#include <stdio.h>
#include <stdint.h>

/* Private declarations */
void write_memory(unsigned int address, unsigned int value);
unsigned int read_memory(unsigned int address);


/* Private Functions */

/* 
 * Write to Memory 
 */
void write_memory(unsigned int address, unsigned int value){

   	volatile unsigned int *p;
    	p=(unsigned int*)(address);
    	*p=value;
}

/* 
 * Read memory 
 */
unsigned int read_memory(unsigned int address){

    	volatile unsigned int *p;
    	volatile unsigned int value;
    	p=(unsigned int*)(address);
    	value=*p;

    	return value;
}

/* Public Functions */

/* 
 * Write Injector Register 
 */
void inj_write_reg(unsigned int entry, unsigned int value){

    	volatile unsigned int *p;
    	p = (unsigned int*)(INJ_BASE_ADDR + (entry*4));
    	*p = value;
}

/* 
 * Read Injector Register 
 */
unsigned int inj_read_reg (unsigned int entry){

	volatile unsigned int *p;
    	volatile unsigned int value;
    	p = (unsigned int*)(INJ_BASE_ADDR + (entry*4));
    	value = *p;

    	return value;
}

/*
 * Start Injector Execution given its configuration
 */
int inj_run(inj_params* params){

	unsigned int inj_reg = 0x00000000;
        unsigned int rsp = 0;
        
	// First descriptor pointer register	
	inj_write_reg(INJ_FDESC_PTR,params->fdesc_ptr);

	inj_reg = inj_reg | (params->ctrl.en);
	inj_reg = inj_reg | (params->ctrl.rst << 1);
	inj_reg = inj_reg | (params->ctrl.int_en << 3);
	inj_reg = inj_reg | (params->ctrl.int_err_en << 4);
	inj_reg = inj_reg | (params->ctrl.q_mode << 5);
	// Control register
        inj_write_reg(INJ_CTRL,inj_reg);
	
	//Check status
	rsp = inj_read_reg(INJ_STATUS);
	rsp = (rsp & 0x00000002) >> 1; //Check Error Bit
	
	return rsp;
}

/*
 * Stop Injector Execution
 */
void inj_stop(void){

        //Control register (0x00) (Reset module)
        inj_write_reg(INJ_CTRL,0x0000001b);
}

/*
 * Setup a descriptor given its parameters
 */
void setup_descriptor(descriptor* descriptor){
        
	unsigned int desc_word = 0x00000000;
	desc_word = desc_word | (descriptor->ctrl.en);			//Enable descriptor bit
        desc_word = desc_word | (descriptor->ctrl.type << 1);  		//Type of descriptor bit (TODO Masked to 3 bits
        desc_word = desc_word | (descriptor->ctrl.irqe << 4);           //Enable Interrupt bit
        desc_word = desc_word | (descriptor->ctrl.addr_fix << 5);    	//Source fix bit
        desc_word = desc_word | (descriptor->ctrl.addr_fix << 6);    	//Destination fix bit
	desc_word = desc_word | (descriptor->ctrl.count << 7);		//Repetition counter (TODO Masked to 6 bits)
        desc_word = desc_word | (descriptor->ctrl.size << 13);

        // Write descriptor to memory
        write_memory(descriptor->desc_addr + 0*4, desc_word);               				//Control word
        write_memory(descriptor->desc_addr + 1*4, (descriptor->next_desc | descriptor->last_desc)); 	//Next descriptor
        write_memory(descriptor->desc_addr + 2*4, (descriptor->dest_addr));                 		//Destination address
        write_memory(descriptor->desc_addr + 3*4, (descriptor->src_addr));       			//Source address (not used for Write Transactions)
}
