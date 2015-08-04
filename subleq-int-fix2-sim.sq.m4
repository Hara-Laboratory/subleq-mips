define(`ARCH',`subleq')
include(`subleq-lib.sq.m4')

@sltu Rd,Rs,Rt
Min Rs; Min Rt;
$(@@sltSub Rd,Rs,Rt,T0,Finish);
Finish:Min Rs; Min Rt; Z Z End;

@multD Hi, Lo, Rs, Rt // destructive
$(@@multDSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, T4, End);

dnl @mult Hi, Lo, Rs, Rt
dnl $(@@multSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, T4, T5, T6, End);

@slt Rd,Rs,Rt
$(@@sltSub Rd,Rs,Rt,T0,End);

@sll Rd, Rt, Sa
$(@@sllsub Rd, Rt, Sa, T0, T1, End);

@srl Rd, Rt, Sa
$(@@srlsub Rd, Rt, Sa, T0, T1, End);

@sra Rd, Rt, Sa
$(@@srasub Rd, Rt, Sa, T0, T1, End);

@multu Hi, Lo, Rs, Rt
$(@@multuSub Hi, Lo, Rs, Rt, T0, T1, T2, T3, End);

@andD Rd, Rs, Rt
$(@@andDSub  Rd, Rs, Rt, T0, T1, End);

@orD Rd, Rs, Rt
$(@@orDSub  Rd, Rs, Rt, T0, T1, End);

@xorD Rd, Rs, Rt
$(@@xorDSub  Rd, Rs, Rt, T0, T1, End);
