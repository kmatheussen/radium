/*
displaygain(i) = _,_ <: _,_,(abs,*(0):+) : _,_,gainview(i) : _,attach;

min_db          = -100.0; // db2linear_from_table(-100) = 0
min_linear_db   = -80.0; // Between min_db and min_linear_db, there is a linear conversion

max_linear_db   = 40.0; // Between max_linear_db and max_db, there is a linear conversion
max_db          = 80.0; // 10000.0 = db2linear_from_table(80) =  db2linear_from_table(81) =  db2linear_from_table(82) = ...

db2linear_from_table(x) = select2(x>min_db,
                                  0.0,
                                  select2(x>min_linear_db,
                                          scale(x,
                                                min_db, min_linear_db,
                                                0.0, db2linear(min_linear_db)),
                                          select2(x<max_db,
                                                  db2linear(max_db),
                                                  select2(x<max_linear_db,
                                                          scale(x,
                                                                max_linear_db, max_db, 
                                                                db2linear(max_linear_db), db2linear(max_db)),
                                                          table_read(x)))))
    with{
    table_read(x)        = rdtable(tablesize, db2linearform, int(scale(x, min_linear_db,max_linear_db, 0,tablesize)));
    db2linearform        = db2linear(scale(time, 0,tablesize, min_linear_db,max_linear_db));
    time                 = (+(1)~_ ) - 1; 			// 0,1,2,3,...
    scale(x,x1,x2,y1,y2) = y1 + (x-x1)*(y2-y1)/(x2-x1);
    tablesize       = 1 << 16;
  };


linear2db_from_table(x) = select2(x>min_linear,
                                  min_db,
                                  select2(x>min_linear_linear,
                                          scale(x,
                                                0.0, min_linear_db,
                                                min_linear, min_linear_linear),
                                          select2(x<max_linear,
                                                  max_db,
                                                  select2(x<max_linear_linear,
                                                          scale(x,
                                                                max_linear_db, max_db,
                                                                max_linear_linear, max_linear),
                                                          table_read(x)))))
    with{
      table_read(x)        = rdtable(tablesize, linear2dbform, int(scale(x, min_linear_linear,max_linear_linear, 0,tablesize)));
      linear2dbform        = linear2db(scale(time, 0,tablesize, min_linear_linear,max_linear_linear));
      time                 = (+(1)~_ ) - 1; 			// 0,1,2,3,...
      scale(x,x1,x2,y1,y2) = y1 + (x-x1)*(y2-y1)/(x2-x1);

      min_linear          = db2linear(min_db);
      min_linear_linear   = db2linear(min_linear_db);
      max_linear_linear   = db2linear(max_linear_db);
      max_linear          = db2linear(max_db);
      tablesize           = 1 << 16;
   };

fasterpow2 = ffunction(float fasterpow2 (float), "typepunning.h", "");
fasterlog = ffunction(float fasterlog (float), "typepunning.h", "");

fasterdb2linear(x) = fasterpow2(1.442695040*x*log(10)/20);
fasterlinear2db(x) = 20*log10(e)*fasterlog(x) with{e=2.71828182845904523536028747135266249;};

quitefastdb2linear(x) = exp(x*log(10.0)/20);
quitefastlinear2db(x) = 20*log10(e)*log(x) with{e=2.71828182845904523536028747135266249;};
*/


// The arithmetic for ll2_pow2 and ll2_log are copied from "fastonebigheader.h" written by
// Paul Mineiro:
// http://www.machinedlearnings.com/2011/06/fast-approximate-logarithm-exponential.html


pun_int_to_float = ffunction(float pun_int_to_float(int), "typepunning.h", "");
pun_float_to_int = ffunction(int pun_float_to_int(float), "typepunning.h", "");

ll2_pow2(x) = pun_int_to_float(( (1 << 23) * (clipp + 126.94269504))) with{
    clipp = max(-126.0,x);
};

ll2_exp(x) = ll2_pow2(1.442695040*x);
ll2_db2linear(x) = ll2_exp(x*log(10.0)/20);

ll2_log(x) = y - 87.989971088 with{
    y = float(pun_float_to_int(x)) * 8.2629582881927490e-8;
};

ll2_linear2db(x) = 20*log10(e)*ll2_log(x) with{e=2.71828182845904523536028747135266249;};

