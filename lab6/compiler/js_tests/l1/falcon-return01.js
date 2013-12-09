function fadd(x, y) {
  return x + y;
}

function fsub(x, y) {
  return x - y;
}

function fmul(x, y) {
  return x * y;
}

function fdiv(x, y) {
  return x / y;
}

function fless(x, y) {
 return (x < y);
}

function itof(x) {
  return x;
}

function ftoi(x) {
  return x;
}

function print_fpt(x) {
  return x;
}

function print_int(x) {
  return x;
}

function print_hex(x) {
  return x;
}

foreignImports = {
  fadd : fadd,
  fsub : fsub,
  fmul : fmul,
  fdiv : fdiv,
  fless : fless,
  itof : itof,
  ftoi : ftoi,
  print_fpt : print_fpt,
  print_int : print_int,
  print_hex : print_hex
}

function polyMul(a, b) {
  var ah  = (a >>> 16) & 0xffff;
  var al = a & 0xffff;
  var bh  = (b >>> 16) & 0xffff;
  var bl = b & 0xffff;
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
}
if (typeof (this.Math.imul) == "undefined") {
  this.Math.imul = polyMul
}
function c0module(stdlib, foreign, heap) {
  "use asm";

  // Global Declarations
  var H32 = new stdlib.Int32Array(heap);
  var g_heapoff = 1;
  var g_stackoff = 0;

  // Global Exception Catchers
  var g_memex = 0;
  var g_oomex = 0;
  var g_numex = 0;
  var g_assex = 0;
  var imul = stdlib.Math.imul;
  var INTMIN = 0x80000000;

  // Function Declarations

  var fadd = foreign.fadd;
  var fsub = foreign.fsub;
  var fmul = foreign.fmul;
  var fdiv = foreign.fdiv;
  var fless = foreign.fless;
  var itof = foreign.itof;
  var ftoi = foreign.ftoi;
  var print_fpt = foreign.print_fpt;
  var print_int = foreign.print_int;
  var print_hex = foreign.print_hex;

  function memAlloc(size) {
    size = size | 0;
    var ret = 0
    if ((g_heapoff | 0) < (H32[0 | 0] | 0)) {
      ret = g_heapoff | 0;
      g_heapoff = (g_heapoff | 0) + (size | 0) | 0;
    } else {
      g_oomex = 1 | 0;
    }
   return ret | 0;
  }

  function memArrAlloc(size, numElems) {
    size = size | 0;
    numElems = numElems | 0;
    size = (size | 0) + (1 | 0) | 0;
    numElems = (size | 0) | 0;
    var ret = 0;
    if ((g_heapoff | 0) < (H32[0 | 0] | 0)) {
      ret = g_heapoff | 0;
      g_heapoff = (g_heapoff | 0) + (size | 0) | 0;
    } else {
      g_oomex = 1 | 0;
    }
    if (((numElems | 0) - (1 | 0) | 0) < (0 | 0)) {
      g_memex = 1 | 0;
    }
    H32[ret | 0] = ((numElems | 0) - (1 | 0)) | 0;
   return ret | 0;
  }

  function stackInit() {
    g_stackoff = (H32[0 | 0] | 0) - 1 | 0;
  }

  function polyDiv(num, den) {
    num = num | 0;
    den = den | 0;
    var ret = 0;
    ret = (num | 0) / (den | 0)| 0;
    if ((den | 0) == (0 | 0)) {
      g_numex = 1;
    }
    if (((num | 0) == (INTMIN | 0)) && ((den | 0) == (-1 | 0))) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function polyMod(num, den) {
    num = num | 0;
    den = den | 0;
    var ret = 0;
    ret = (num | 0) % (den | 0)| 0;
    if ((den | 0) == (0 | 0)) {
      g_numex = 1;
    }
    if (((num | 0) == (INTMIN | 0)) && ((den | 0) == (-1 | 0))) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function polyRShift(l, r) {
    l = l | 0;
    r = r | 0;
    var ret = 0;
    ret = (l | 0) >> (r | 0) | 0;
    if ((r | 0) >= (32 | 0)) {
      g_numex = 1;
    }
    if ((r | 0) < (0 | 0)) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function polyLShift(l, r) {
    l = l | 0;
    r = r | 0;
    var ret = 0;
    ret = (l | 0) << (r | 0) | 0;
    if ((r | 0) >= (32 | 0)) {
      g_numex = 1;
    }
    if ((r | 0) < (0 | 0)) {
      g_numex = 1;
    }
    return ret | 0;
  }


  function pointerDeref(loc) {
    loc = loc | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return loc | 0;
  }

  function pointerLoad(loc) {
    loc = loc | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return H32[loc | 0] | 0;
  }

  function fieldAccess(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return H32[loc + off | 0] | 0;
  }


  function arrAccess(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((off | 0) < (0 | 0)) {
      g_memex = 1;
    }
    if ((off | 0) >= (H32[loc | 0] | 0)) {
      g_memex = 1 | 0;
    }
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return H32[loc + off + (1 | 0)] | 0;
  }


  function fieldShift(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return (loc | 0) + (off | 0) | 0;
  }


  function arrShift(loc, off) {
    loc = loc | 0;
    off = off | 0;
    if ((off | 0) < (0 | 0)) {
      g_memex = 1;
    }
    if ((off | 0) >= (H32[loc | 0] | 0)) {
      g_memex = 1;
    }
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    return (loc | 0) + (off | 0) + (1 | 0) | 0;
  }


  function memSet(loc, val) {
    loc = loc | 0;
    val = val | 0;
    if ((loc | 0) == (0 | 0)) {
      g_memex = 1;
    }
    H32[loc | 0] = val | 0;
    return;
  }


  function assert(e) {
    e = e | 0
    if ((e | 0) != (1 | 0)) {
      g_assex = 1;
    }
    return;
  }


  function _c0_main () {
    var temp_ptr = 0;
    var c0_var_test1 = 0;
    var c0_var_test10 = 0;
    var c0_var_test11 = 0;
    var c0_var_test12 = 0;
    var c0_var_test13 = 0;
    var c0_var_test14 = 0;
    var c0_var_test15 = 0;
    var c0_var_test16 = 0;
    var c0_var_test17 = 0;
    var c0_var_test18 = 0;
    var c0_var_test19 = 0;
    var c0_var_test2 = 0;
    var c0_var_test20 = 0;
    var c0_var_test21 = 0;
    var c0_var_test22 = 0;
    var c0_var_test23 = 0;
    var c0_var_test24 = 0;
    var c0_var_test25 = 0;
    var c0_var_test26 = 0;
    var c0_var_test27 = 0;
    var c0_var_test28 = 0;
    var c0_var_test29 = 0;
    var c0_var_test3 = 0;
    var c0_var_test30 = 0;
    var c0_var_test31 = 0;
    var c0_var_test32 = 0;
    var c0_var_test33 = 0;
    var c0_var_test34 = 0;
    var c0_var_test35 = 0;
    var c0_var_test36 = 0;
    var c0_var_test37 = 0;
    var c0_var_test38 = 0;
    var c0_var_test39 = 0;
    var c0_var_test4 = 0;
    var c0_var_test40 = 0;
    var c0_var_test41 = 0;
    var c0_var_test42 = 0;
    var c0_var_test43 = 0;
    var c0_var_test44 = 0;
    var c0_var_test45 = 0;
    var c0_var_test46 = 0;
    var c0_var_test47 = 0;
    var c0_var_test48 = 0;
    var c0_var_test49 = 0;
    var c0_var_test5 = 0;
    var c0_var_test50 = 0;
    var c0_var_test51 = 0;
    var c0_var_test52 = 0;
    var c0_var_test53 = 0;
    var c0_var_test54 = 0;
    var c0_var_test55 = 0;
    var c0_var_test56 = 0;
    var c0_var_test57 = 0;
    var c0_var_test58 = 0;
    var c0_var_test59 = 0;
    var c0_var_test6 = 0;
    var c0_var_test60 = 0;
    var c0_var_test61 = 0;
    var c0_var_test62 = 0;
    var c0_var_test63 = 0;
    var c0_var_test64 = 0;
    var c0_var_test65 = 0;
    var c0_var_test66 = 0;
    var c0_var_test67 = 0;
    var c0_var_test68 = 0;
    var c0_var_test69 = 0;
    var c0_var_test7 = 0;
    var c0_var_test70 = 0;
    var c0_var_test71 = 0;
    var c0_var_test72 = 0;
    var c0_var_test73 = 0;
    var c0_var_test74 = 0;
    var c0_var_test75 = 0;
    var c0_var_test76 = 0;
    var c0_var_test77 = 0;
    var c0_var_test78 = 0;
    var c0_var_test79 = 0;
    var c0_var_test8 = 0;
    var c0_var_test80 = 0;
    var c0_var_test81 = 0;
    var c0_var_test82 = 0;
    var c0_var_test83 = 0;
    var c0_var_test84 = 0;
    var c0_var_test85 = 0;
    var c0_var_test86 = 0;
    var c0_var_test87 = 0;
    var c0_var_test88 = 0;
    var c0_var_test89 = 0;
    var c0_var_test9 = 0;
    var c0_var_test90 = 0;
    var c0_var_test91 = 0;
    var c0_var_test92 = 0;
    var c0_var_test93 = 0;
    var c0_var_test94 = 0;
    var c0_var_test95 = 0;
    var c0_var_test96 = 0;
    var c0_var_test97 = 0;
    var c0_var_test98 = 0;
    var c0_var_test99 = 0;
     c0_var_test1  =  1 | 0;
     c0_var_test2  =  2 | 0;
     c0_var_test3  =  3 | 0;
     c0_var_test4  =  4 | 0;
     c0_var_test5  =  5 | 0;
     c0_var_test6  =  6 | 0;
     c0_var_test7  =  7 | 0;
     c0_var_test8  =  8 | 0;
     c0_var_test9  =  9 | 0;
     c0_var_test10  =  10 | 0;
     c0_var_test11  =  1 | 0;
     c0_var_test12  =  1 | 0;
     c0_var_test13  =  1 | 0;
     c0_var_test14  =  1 | 0;
     c0_var_test15  =  1 | 0;
     c0_var_test16  =  1 | 0;
     c0_var_test17  =  1 | 0;
     c0_var_test18  =  1 | 0;
     c0_var_test19  =  1 | 0;
     c0_var_test20  =  1 | 0;
     c0_var_test21  =  1 | 0;
     c0_var_test22  =  1 | 0;
     c0_var_test23  =  1 | 0;
     c0_var_test24  =  1 | 0;
     c0_var_test25  =  1 | 0;
     c0_var_test26  =  1 | 0;
     c0_var_test27  =  1 | 0;
     c0_var_test28  =  1 | 0;
     c0_var_test29  =  1 | 0;
     c0_var_test30  =  1 | 0;
     c0_var_test31  =  1 | 0;
     c0_var_test32  =  1 | 0;
     c0_var_test33  =  1 | 0;
     c0_var_test34  =  1 | 0;
     c0_var_test35  =  1 | 0;
     c0_var_test36  =  1 | 0;
     c0_var_test37  =  1 | 0;
     c0_var_test38  =  1 | 0;
     c0_var_test39  =  1 | 0;
     c0_var_test40  =  1 | 0;
     c0_var_test41  =  1 | 0;
     c0_var_test42  =  1 | 0;
     c0_var_test43  =  1 | 0;
     c0_var_test44  =  1 | 0;
     c0_var_test45  =  1 | 0;
     c0_var_test46  =  1 | 0;
     c0_var_test47  =  1 | 0;
     c0_var_test48  =  1 | 0;
     c0_var_test49  =  1 | 0;
     c0_var_test50  =  1 | 0;
     c0_var_test51  =  1 | 0;
     c0_var_test52  =  1 | 0;
     c0_var_test53  =  1 | 0;
     c0_var_test54  =  1 | 0;
     c0_var_test55  =  1 | 0;
     c0_var_test56  =  1 | 0;
     c0_var_test57  =  1 | 0;
     c0_var_test58  =  1 | 0;
     c0_var_test59  =  1 | 0;
     c0_var_test60  =  1 | 0;
     c0_var_test61  =  1 | 0;
     c0_var_test62  =  1 | 0;
     c0_var_test63  =  1 | 0;
     c0_var_test64  =  1 | 0;
     c0_var_test65  =  1 | 0;
     c0_var_test66  =  1 | 0;
     c0_var_test67  =  1 | 0;
     c0_var_test68  =  1 | 0;
     c0_var_test69  =  1 | 0;
     c0_var_test70  =  1 | 0;
     c0_var_test71  =  1 | 0;
     c0_var_test72  =  1 | 0;
     c0_var_test73  =  1 | 0;
     c0_var_test74  =  1 | 0;
     c0_var_test75  =  1 | 0;
     c0_var_test76  =  1 | 0;
     c0_var_test77  =  1 | 0;
     c0_var_test78  =  1 | 0;
     c0_var_test79  =  1 | 0;
     c0_var_test80  =  1 | 0;
     c0_var_test81  =  1 | 0;
     c0_var_test82  =  1 | 0;
     c0_var_test83  =  1 | 0;
     c0_var_test84  =  1 | 0;
     c0_var_test85  =  1 | 0;
     c0_var_test86  =  1 | 0;
     c0_var_test87  =  1 | 0;
     c0_var_test88  =  1 | 0;
     c0_var_test89  =  1 | 0;
     c0_var_test90  =  1 | 0;
     c0_var_test91  =  1 | 0;
     c0_var_test92  =  1 | 0;
     c0_var_test93  =  1 | 0;
     c0_var_test94  =  1 | 0;
     c0_var_test95  =  1 | 0;
     c0_var_test96  =  1 | 0;
     c0_var_test97  =  1 | 0;
     c0_var_test98  =  1 | 0;
     c0_var_test99  =  1 | 0;
    return (imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul(imul( 1  | 0, 2  | 0) | 0 | 0, 3  | 0) | 0 | 0, 4  | 0) | 0 | 0, 5  | 0) | 0 | 0, 6  | 0) | 0 | 0, 7  | 0) | 0 | 0, 8  | 0) | 0 | 0, 9  | 0) | 0 | 0, 10  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0, 1  | 0) | 0 | 0);

  }

  function main() {
    stackInit();
    return _c0_main() | 0;
  }

  function getNumEx() {
    return g_numex | 0;
  }
  function getMemEx() {
    return g_memex | 0;
  }
  function getOomEx() {
    return g_oomex | 0;
  }
  function getAssEx() {
    return g_assex | 0;
  }
  return { main : main , getNumEx : getNumEx, getMemEx : getMemEx, getOomEx : getOomEx, getAssEx : getAssEx}

}
var c0arr = new Int32Array(5000000)
c0arr[0] = 5000000;
var c0_export = c0module(this, foreignImports, c0arr);
var res = (c0_export.main())
var numEx = (c0_export.getNumEx())
var memEx = (c0_export.getMemEx())
var assEx = (c0_export.getAssEx())
print("Result: " + res)
print("NumEx: " + numEx)
print("MemEx: " + memEx)
print("AssEx: " + assEx)