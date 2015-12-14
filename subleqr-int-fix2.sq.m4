define(`ARCH',`subleqr')
include(`subleq-lib.sq.m4')

@srl1dTest Rh, Rl
$(@@sl1d Rh, Rl, End);

@mflo Rd,Lo
Rd;
Lo Z;
Z Rd;
Z Z End;

@mtlo Lo,Rs
Lo;
Rs Z;
Z Lo;
Z Z End;

@sltu Rd,Rs,Rt
Min Rs; Min Rt;
$(@@sltSub Rd,Rs,Rt,T0,Finish);
Finish:Min Rs; Min Rt; Z Z End;

@multD Hi, Lo, Rs, Rt // destructive
$(@@multDSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, T4, End);

@mult Hi, Lo, Rs, Rt
$(@@multSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, T4, T5, T6, End);

@slt Rd,Rs,Rt
$(@@sltSub Rd,Rs,Rt,T0,End);

@syscall Rd
Rd;
2 Z;
Z Rd;
Z Z End;

@addu Rd,Rs,Rt
$(@@add Rd, Rs, Rt, End);

@subu Rd,Rs,Rt
Rs Z;
Rt T0;
T0 Z;
T0;
Rd;
Z Rd;
Z Z End;

@shl1 Rd,Rs
Rs Z; // Z <- -Rs, it assumes Z = 0
Rs Z; // Z <- Z - Rt = -(Rs + Rt)
Rd; // clear Rd
Z Rd;
Z Z End;

@bne Rs, Rt, Offset, PCq // I assume Offset is already sign-extended.
	Rs T1; T1 T2; Rt T2;
	Inc T2 LFinish;
	Dec T2 LTaken;
	LFinish:T1; T2 T2 End;
	LTaken:Offset Z; Z PCq; Z Z LFinish;

@bnea Rs, Offset, PCq // I assume Offset is already sign-extended.
	Rs T1; T1 T2; Rs T2;
	Inc T2 LFinish;
	Dec T2 LTaken;
	LFinish:T1; T2 T2 End;
	LTaken:Offset Z; Z PCq; Z Z LFinish;

@sll Rd, Rt, Sa
$(@@sllsub Rd, Rt, Sa, T0, T1, End);

@slli3 Rd, Rt, Sa
$(@@slli3sub Rd, Rt, Sa, T0, T1, End);

@slli4 Rd, Rt, Sa
$(@@slli4sub Rd, Rt, Sa, T0, T1, End);

@srl Rd, Rt, Sa
$(@@srlsub  Rd, Rt, Sa, T0, T1, End);

@srli3 Rd, Rt, Sa
$(@@srli3sub  Rd, Rt, Sa, T0, T1, End);

@srli4 Rd, Rt, Sa
$(@@srli4sub  Rd, Rt, Sa, T0, T1, End);

@srlS Rd, Rt, Sa // Original Subleq
Rt T1; Rd;
Sa T0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; T0 Sa; T0; Rt; T1 Rt; T1 T1 End;
LBody:$(@@sl1d Rd, Rt, Loop);

@sraO Rd, Rt, Sa
Rt T1; Rd;
$(@@jnzp Rt, Ln, Lzp, Lzp);
Ln:Dec Rd;
Lzp:Sa T0; CW Sa;
Loop:Inc Sa LBody;
LFinish:Sa; T0 Sa; T0; Rt; T1 Rt; T1 T1 End;
LBody:$(@@sl1d Rd, Rt, Loop);

@sra Rd, Rt, Sa
$(@@srasub Rd, Rt, Sa, T0, T1, T2, End);

@srl1dcTest Rd, Rh, Rl
$(@@sl1dc Rd, Rh, Rl, End);

@srl1Test Rd, Rs
$(@@srl1 Rd, Rs, End);

@multu Hi, Lo, Rs, Rt
$(@@multuSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, End);

@jezoTest R
$(@@jezo R, Le, Lz, Lo);
Le:R; Inc R End; Z Z End;
Lz:R; Z Z End;
Lo:R; Dec R End; Z Z End;

@and Rd, Rs, Rt
Rs T2; Rt T3;
$(@@andDSub  Rd, Rs, Rt, T0, T1, LFinish);
LFinish:Rs; Rt; T2 Rs; T3 Rt; T2; T3 T3 End;

@or Rd, Rs, Rt
Rs T2; Rt T3;
$(@@orDSub  Rd, Rs, Rt, T0, T1, LFinish);
LFinish:Rs; Rt; T2 Rs; T3 Rt; T2; T3 T3 End;

@xor Rd, Rs, Rt
Rs T2; Rt T3;
$(@@xorDSub  Rd, Rs, Rt, T0, T1, LFinish);
LFinish:Rs; Rt; T2 Rs; T3 Rt; T2; T3 T3 End;

@nor Rd, Rs, Rt
Rs T2; Rt T3;
$(@@norDSub  Rd, Rs, Rt, T0, T1, LFinish);
LFinish:Rs; Rt; T2 Rs; T3 Rt; T2; T3 T3 End;

@not Rd, Rs, Rt
$(@@inv  Rd, Rs, T0, End);

@lwTest Rt, Rs
$(@@lwSub Rt, Zero, One, T0, T1, T2, T3, T4, End);
Zero:0 One:4;

@swTest Rt, Rs
$(@@swSub Rt, Zero, One, T0, T1, T2, T3, T4, End);
Zero:0 One:4;

@lbuTest Rt, Rs, Offset
$(@@lbuSub Rt, One, Offset, T0, T1, T2, T3, T4, T5, T6, T7, End);
Zero:0 One:4 T7:0;

@sbTest Rt, Rs, Offset
$(@@sbSub Rt, One, Offset, T0, T1, T2, T3, T4, T5, T6, T7, End);
Zero:0 One:4 T7:0;

@lhuTest Rt, Rs, Offset
$(@@lhuSub Rt, One, Offset, T0, T1, T2, T3, T4, T5, T6, T7, End);
Zero:0 One:4 T7:0;

@shTest Rt, Rs, Offset
$(@@shSub Rt, One, Offset, T0, T1, T2, T3, T4, T5, T6, T7, End);
Zero:0 One:4 T7:0;

@lui Rd, Imm
$(@@luiSub  Rd, Imm, T0, T1, End);

@rrTest Rd, Sa
$(@@rr  Rd, Sa, T0, T1, End);

@rrsrmTest Rd, Rs, Sa
$(@@rrsrm Rd, Rs, Sa, T0, T1, End);

@rlTest Rd, Sa
$(@@rl  Rd, Sa, T0, T1, End);

@rlslmTest Rd, Rs, Sa
$(@@rlslm Rd, Rs, Sa, T0, T1, End);

@sviTest Rd, Rs, From, Size
$(@@svi Rd, Rs, From, Size, T0, T1, T2, T3, End);

@svliTest Rd, Rs, From, Size
$(@@svli Rd, Rs, From, Size, T0, T1, T2, T3, End);

@svriTest Rd, Rs, From, Size
$(@@svri Rd, Rs, From, Size, T0, T1, T2, T3, End);

@lvuiTest Rd, Rs, From, Size
$(@@lvui Rd, Rs, From, Size, T0, T1, T2, T3, End);

@lvuliTest Rd, Rs, From, Size
$(@@lvuli Rd, Rs, From, Size, T0, T1, T2, T3, End);

@lvuriTest Rd, Rs, From, Size
$(@@lvuri Rd, Rs, From, Size, T0, T1, T2, T3, End);


@shiTest Rd, Rs, From
$(@@shi Rd, Rs, From, T0, T1, T2, T3, End);

@shliTest Rd, Rs, From
$(@@shli Rd, Rs, From, T0, T1, T2, T3, End);

@shriTest Rd, Rs, From
$(@@shri Rd, Rs, From, T0, T1, T2, T3, End);

@lhuiTest Rd, Rs, From
$(@@lhui Rd, Rs, From, T0, T1, T2, T3, End);

@lhuliTest Rd, Rs, From
$(@@lhuli Rd, Rs, From, T0, T1, T2, T3, End);

@lhuriTest Rd, Rs, From
$(@@lhuri Rd, Rs, From, T0, T1, T2, T3, End);


@sbiTest Rd, Rs, From
$(@@sbi Rd, Rs, From, T0, T1, T2, T3, End);

@sbliTest Rd, Rs, From
$(@@sbli Rd, Rs, From, T0, T1, T2, T3, End);

@sbriTest Rd, Rs, From
$(@@sbri Rd, Rs, From, T0, T1, T2, T3, End);

@lbuiTest Rd, Rs, From
$(@@lbui Rd, Rs, From, T0, T1, T2, T3, End);

@lbuliTest Rd, Rs, From
$(@@lbuli Rd, Rs, From, T0, T1, T2, T3, End);

@lburiTest Rd, Rs, From
$(@@lburi Rd, Rs, From, T0, T1, T2, T3, End);

@addrwTest Rd, Rt, Rs
$(@@addrw Rd, Rt, Rs, T0, End);

@addrhTest Rd, Rt, Rs
$(@@addrh Rd, Rt, Rs, T0, End);

@addrbTest Rd, Rt, Rs
$(@@addrb Rd, Rt, Rs, T0, End);

