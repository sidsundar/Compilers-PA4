# start of generated code
	.data
	.align	2
	.globl	class_nameTab
	.globl	Main_protObj
	.globl	Int_protObj
	.globl	String_protObj
	.globl	bool_const0
	.globl	bool_const1
	.globl	_int_tag
	.globl	_bool_tag
	.globl	_string_tag
_int_tag:
	.word	1
_bool_tag:
	.word	2
_string_tag:
	.word	0
	.globl	_MemMgr_INITIALIZER
_MemMgr_INITIALIZER:
	.word	_NoGC_Init
	.globl	_MemMgr_COLLECTOR
_MemMgr_COLLECTOR:
	.word	_NoGC_Collect
	.globl	_MemMgr_TEST
_MemMgr_TEST:
	.word	0
	.word	-1
String_protObj:
str_const14:
	.word	0
	.word	5
	.word	String_dispTab
	.word	int_const0
	.byte	0	
	.align	2
	.word	-1
str_const13:
	.word	0
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"Alpha"
	.byte	0	
	.align	2
	.word	-1
str_const12:
	.word	0
	.word	7
	.word	String_dispTab
	.word	int_const2
	.ascii	"Hullaboola"
	.byte	0	
	.align	2
	.word	-1
str_const11:
	.word	0
	.word	6
	.word	String_dispTab
	.word	int_const3
	.ascii	"Main"
	.byte	0	
	.align	2
	.word	-1
str_const10:
	.word	0
	.word	6
	.word	String_dispTab
	.word	int_const4
	.ascii	"String"
	.byte	0	
	.align	2
	.word	-1
str_const9:
	.word	0
	.word	6
	.word	String_dispTab
	.word	int_const3
	.ascii	"Bool"
	.byte	0	
	.align	2
	.word	-1
str_const8:
	.word	0
	.word	5
	.word	String_dispTab
	.word	int_const5
	.ascii	"Int"
	.byte	0	
	.align	2
	.word	-1
str_const7:
	.word	0
	.word	5
	.word	String_dispTab
	.word	int_const6
	.ascii	"IO"
	.byte	0	
	.align	2
	.word	-1
str_const6:
	.word	0
	.word	6
	.word	String_dispTab
	.word	int_const4
	.ascii	"Object"
	.byte	0	
	.align	2
	.word	-1
str_const5:
	.word	0
	.word	7
	.word	String_dispTab
	.word	int_const2
	.ascii	"_prim_slot"
	.byte	0	
	.align	2
	.word	-1
str_const4:
	.word	0
	.word	7
	.word	String_dispTab
	.word	int_const7
	.ascii	"SELF_TYPE"
	.byte	0	
	.align	2
	.word	-1
str_const3:
	.word	0
	.word	7
	.word	String_dispTab
	.word	int_const7
	.ascii	"_no_class"
	.byte	0	
	.align	2
	.word	-1
str_const2:
	.word	0
	.word	8
	.word	String_dispTab
	.word	int_const8
	.ascii	"<basic class>"
	.byte	0	
	.align	2
	.word	-1
str_const1:
	.word	0
	.word	8
	.word	String_dispTab
	.word	int_const8
	.ascii	"Hello world!\n"
	.byte	0	
	.align	2
	.word	-1
str_const0:
	.word	0
	.word	7
	.word	String_dispTab
	.word	int_const2
	.ascii	"example.cl"
	.byte	0	
	.align	2
	.word	-1
int_const8:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	13
	.word	-1
int_const7:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	9
	.word	-1
int_const6:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	2
	.word	-1
int_const5:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	3
	.word	-1
int_const4:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	6
	.word	-1
int_const3:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	4
	.word	-1
int_const2:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	10
	.word	-1
int_const1:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	5
	.word	-1
Int_protObj:
int_const0:
	.word	1
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
Bool_protObj:
bool_const0:
	.word	2
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
bool_const1:
	.word	2
	.word	4
	.word	Bool_dispTab
	.word	1

	.align	2
	.word	-1
Alpha_protObj:
	.word	7
	.word	2
	.word	Alpha_dispTab
	.word	Int_protObj
	.word	Int_protObj
	.word	-1
Hullaboola_protObj:
	.word	6
	.word	0
	.word	Hullaboola_dispTab
	.word	-1
Main_protObj:
	.word	5
	.word	6
	.word	Main_dispTab
	.word	Int_protObj
	.word	Int_protObj
	.word	Int_protObj
	.word	String_protObj
	.word	Bool_protObj
	.word	0
	.word	-1
IO_protObj:
	.word	4
	.word	0
	.word	IO_dispTab
	.word	-1
Object_protObj:
	.word	3
	.word	0
	.word	Object_dispTab
	.align	2
class_nameTab:
	.word	str_const10
	.word	str_const8
	.word	str_const9
	.word	str_const6
	.word	str_const7
	.word	str_const11
	.word	str_const12
	.word	str_const13

	.align	2
Alpha_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
Hullaboola_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
Main_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
	.word	Main.main
String_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
	.word	String.length_pre
	.word	String.concat_pre
	.word	String.substr_pre
Bool_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
Int_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
IO_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
	.word	IO.out_string_pre
	.word	IO.out_int_pre
	.word	IO.in_string_pre
	.word	IO.in_int_pre
Object_dispTab:
	.word	Object.abort_pre
	.word	Object.type_name_pre
	.word	Object.copy_pre
	.globl	heap_start
heap_start:
	.word	0
	.text
	.globl	Main_init
	.globl	Int_init
	.globl	String_init
	.globl	Bool_init
	.globl	Main.main
Alpha_init:
	jr	$ra	
Hullaboola_init:
	jr	$ra	
Main_init:
	jr	$ra	
Main.main:
	sw	$fp 0($sp)
	addiu	$sp $sp -4
	move	$fp $sp
	la	$a1 IO_protObj
	jal	Object.copy
	move	$a0 $a1
	jal	IO_init
	sw	$s0 0($sp)
	addiu	$sp $sp -4
	move	$s0 $a0
	la	$a0 str_const1
	sw	$a0 0($sp)
	addiu	$sp $sp -4
	lw	$t1 8($s0)
	lw	$t1 12($t1)
	jalr		$t1
	addiu	$sp $sp 4
	addiu	$sp $sp 4
	lw	$s0 0($sp)
	move	$sp $fp
	addiu	$sp $sp 4
	lw	$fp 0($sp)
	jr	$ra	
String_init:
	jr	$ra	
Bool_init:
	jr	$ra	
Int_init:
	jr	$ra	
IO_init:
	jr	$ra	
Object_init:
	jr	$ra	

# end of generated code
