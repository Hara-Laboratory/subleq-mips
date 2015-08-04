define(`ARCH',`subleq')
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
Rs Z; // Z <- -Rs, it assumes Z = 0
Rt Z; // Z <- Z - Rt = -(Rs + Rt)
Rd; // clear Rd
Z Rd;
Z Z End;

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

@srl Rd, Rt, Sa
$(@@srlsub  Rd, Rt, Sa, T0, T1, End);

@sra Rd, Rt, Sa
$(@@srasub  Rd, Rt, Sa, T0, T1, End);

@srl1dcTest Rd, Rh, Rl
$(@@sl1dc Rd, Rh, Rl, End);

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

@not Rd, Rs, Rt
$(@@inv  Rd, Rs, T0, End);

@lwTest Rt, Rs
$(@@lwSub Rt, Zero, One, End);
Zero:0 One:4;

@swTest Rt, Rs
$(@@swSub Rt, Zero, One, End);
Zero:0 One:4;

@multu Hi, Lo, Rs, Rt
$(@@multuSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, End);

