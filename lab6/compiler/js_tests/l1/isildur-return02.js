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
    var c0_var_a1 = 0;
    var c0_var_a10 = 0;
    var c0_var_a100 = 0;
    var c0_var_a11 = 0;
    var c0_var_a12 = 0;
    var c0_var_a13 = 0;
    var c0_var_a14 = 0;
    var c0_var_a15 = 0;
    var c0_var_a16 = 0;
    var c0_var_a17 = 0;
    var c0_var_a18 = 0;
    var c0_var_a19 = 0;
    var c0_var_a2 = 0;
    var c0_var_a20 = 0;
    var c0_var_a21 = 0;
    var c0_var_a22 = 0;
    var c0_var_a23 = 0;
    var c0_var_a24 = 0;
    var c0_var_a25 = 0;
    var c0_var_a26 = 0;
    var c0_var_a27 = 0;
    var c0_var_a28 = 0;
    var c0_var_a29 = 0;
    var c0_var_a3 = 0;
    var c0_var_a30 = 0;
    var c0_var_a31 = 0;
    var c0_var_a32 = 0;
    var c0_var_a33 = 0;
    var c0_var_a34 = 0;
    var c0_var_a35 = 0;
    var c0_var_a36 = 0;
    var c0_var_a37 = 0;
    var c0_var_a38 = 0;
    var c0_var_a39 = 0;
    var c0_var_a4 = 0;
    var c0_var_a40 = 0;
    var c0_var_a41 = 0;
    var c0_var_a42 = 0;
    var c0_var_a43 = 0;
    var c0_var_a44 = 0;
    var c0_var_a45 = 0;
    var c0_var_a46 = 0;
    var c0_var_a47 = 0;
    var c0_var_a48 = 0;
    var c0_var_a49 = 0;
    var c0_var_a5 = 0;
    var c0_var_a50 = 0;
    var c0_var_a51 = 0;
    var c0_var_a52 = 0;
    var c0_var_a53 = 0;
    var c0_var_a54 = 0;
    var c0_var_a55 = 0;
    var c0_var_a56 = 0;
    var c0_var_a57 = 0;
    var c0_var_a58 = 0;
    var c0_var_a59 = 0;
    var c0_var_a6 = 0;
    var c0_var_a60 = 0;
    var c0_var_a61 = 0;
    var c0_var_a62 = 0;
    var c0_var_a63 = 0;
    var c0_var_a64 = 0;
    var c0_var_a65 = 0;
    var c0_var_a66 = 0;
    var c0_var_a67 = 0;
    var c0_var_a68 = 0;
    var c0_var_a69 = 0;
    var c0_var_a7 = 0;
    var c0_var_a70 = 0;
    var c0_var_a71 = 0;
    var c0_var_a72 = 0;
    var c0_var_a73 = 0;
    var c0_var_a74 = 0;
    var c0_var_a75 = 0;
    var c0_var_a76 = 0;
    var c0_var_a77 = 0;
    var c0_var_a78 = 0;
    var c0_var_a79 = 0;
    var c0_var_a8 = 0;
    var c0_var_a80 = 0;
    var c0_var_a81 = 0;
    var c0_var_a82 = 0;
    var c0_var_a83 = 0;
    var c0_var_a84 = 0;
    var c0_var_a85 = 0;
    var c0_var_a86 = 0;
    var c0_var_a87 = 0;
    var c0_var_a88 = 0;
    var c0_var_a89 = 0;
    var c0_var_a9 = 0;
    var c0_var_a90 = 0;
    var c0_var_a91 = 0;
    var c0_var_a92 = 0;
    var c0_var_a93 = 0;
    var c0_var_a94 = 0;
    var c0_var_a95 = 0;
    var c0_var_a96 = 0;
    var c0_var_a97 = 0;
    var c0_var_a98 = 0;
    var c0_var_a99 = 0;
     c0_var_a1  =  1 | 0;
     c0_var_a2  =  2 | 0;
     c0_var_a3  =  3 | 0;
     c0_var_a4  =  4 | 0;
     c0_var_a5  =  5 | 0;
     c0_var_a6  =  6 | 0;
     c0_var_a7  =  7 | 0;
     c0_var_a8  =  8 | 0;
     c0_var_a9  =  9 | 0;
     c0_var_a10  =  10 | 0;
     c0_var_a11  =  11 | 0;
     c0_var_a12  =  12 | 0;
     c0_var_a13  =  13 | 0;
     c0_var_a14  =  14 | 0;
     c0_var_a15  =  15 | 0;
     c0_var_a16  =  16 | 0;
     c0_var_a17  =  17 | 0;
     c0_var_a18  =  18 | 0;
     c0_var_a19  =  19 | 0;
     c0_var_a20  =  20 | 0;
     c0_var_a21  =  21 | 0;
     c0_var_a22  =  22 | 0;
     c0_var_a23  =  23 | 0;
     c0_var_a24  =  24 | 0;
     c0_var_a25  =  25 | 0;
     c0_var_a26  =  26 | 0;
     c0_var_a27  =  27 | 0;
     c0_var_a28  =  28 | 0;
     c0_var_a29  =  29 | 0;
     c0_var_a30  =  30 | 0;
     c0_var_a31  =  31 | 0;
     c0_var_a32  =  32 | 0;
     c0_var_a33  =  33 | 0;
     c0_var_a34  =  34 | 0;
     c0_var_a35  =  35 | 0;
     c0_var_a36  =  36 | 0;
     c0_var_a37  =  37 | 0;
     c0_var_a38  =  38 | 0;
     c0_var_a39  =  39 | 0;
     c0_var_a40  =  40 | 0;
     c0_var_a41  =  41 | 0;
     c0_var_a42  =  42 | 0;
     c0_var_a43  =  43 | 0;
     c0_var_a44  =  44 | 0;
     c0_var_a45  =  45 | 0;
     c0_var_a46  =  46 | 0;
     c0_var_a47  =  47 | 0;
     c0_var_a48  =  48 | 0;
     c0_var_a49  =  49 | 0;
     c0_var_a50  =  50 | 0;
     c0_var_a51  =  51 | 0;
     c0_var_a52  =  52 | 0;
     c0_var_a53  =  53 | 0;
     c0_var_a54  =  54 | 0;
     c0_var_a55  =  55 | 0;
     c0_var_a56  =  56 | 0;
     c0_var_a57  =  57 | 0;
     c0_var_a58  =  58 | 0;
     c0_var_a59  =  59 | 0;
     c0_var_a60  =  60 | 0;
     c0_var_a61  =  61 | 0;
     c0_var_a62  =  62 | 0;
     c0_var_a63  =  63 | 0;
     c0_var_a64  =  64 | 0;
     c0_var_a65  =  65 | 0;
     c0_var_a66  =  66 | 0;
     c0_var_a67  =  67 | 0;
     c0_var_a68  =  68 | 0;
     c0_var_a69  =  69 | 0;
     c0_var_a70  =  70 | 0;
     c0_var_a71  =  71 | 0;
     c0_var_a72  =  72 | 0;
     c0_var_a73  =  73 | 0;
     c0_var_a74  =  74 | 0;
     c0_var_a75  =  75 | 0;
     c0_var_a76  =  76 | 0;
     c0_var_a77  =  77 | 0;
     c0_var_a78  =  78 | 0;
     c0_var_a79  =  79 | 0;
     c0_var_a80  =  80 | 0;
     c0_var_a81  =  81 | 0;
     c0_var_a82  =  82 | 0;
     c0_var_a83  =  83 | 0;
     c0_var_a84  =  84 | 0;
     c0_var_a85  =  85 | 0;
     c0_var_a86  =  86 | 0;
     c0_var_a87  =  87 | 0;
     c0_var_a88  =  88 | 0;
     c0_var_a89  =  89 | 0;
     c0_var_a90  =  90 | 0;
     c0_var_a91  =  91 | 0;
     c0_var_a92  =  92 | 0;
     c0_var_a93  =  93 | 0;
     c0_var_a94  =  94 | 0;
     c0_var_a95  =  95 | 0;
     c0_var_a96  =  96 | 0;
     c0_var_a97  =  97 | 0;
     c0_var_a98  =  98 | 0;
     c0_var_a99  =  99 | 0;
     c0_var_a100  =  100 | 0;
    return (polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv(polyDiv( 100  | 0, 99  | 0) | 0 | 0, 98  | 0) | 0 | 0, 97  | 0) | 0 | 0, 96  | 0) | 0 | 0, 95  | 0) | 0 | 0, 94  | 0) | 0 | 0, 93  | 0) | 0 | 0, 92  | 0) | 0 | 0, 91  | 0) | 0 | 0, 90  | 0) | 0 | 0, 89  | 0) | 0 | 0, 88  | 0) | 0 | 0, 87  | 0) | 0 | 0, 86  | 0) | 0 | 0, 85  | 0) | 0 | 0, 84  | 0) | 0 | 0, 83  | 0) | 0 | 0, 82  | 0) | 0 | 0, 81  | 0) | 0 | 0, 80  | 0) | 0 | 0, 79  | 0) | 0 | 0, 78  | 0) | 0 | 0, 77  | 0) | 0 | 0, 76  | 0) | 0 | 0, 75  | 0) | 0 | 0, 74  | 0) | 0 | 0, 73  | 0) | 0 | 0, 72  | 0) | 0 | 0, 71  | 0) | 0 | 0, 70  | 0) | 0 | 0, 69  | 0) | 0 | 0, 68  | 0) | 0 | 0, 67  | 0) | 0 | 0, 66  | 0) | 0 | 0, 65  | 0) | 0 | 0, 64  | 0) | 0 | 0, 63  | 0) | 0 | 0, 62  | 0) | 0 | 0, 61  | 0) | 0 | 0, 60  | 0) | 0 | 0, 59  | 0) | 0 | 0, 58  | 0) | 0 | 0, 57  | 0) | 0 | 0, 56  | 0) | 0 | 0, 55  | 0) | 0 | 0, 54  | 0) | 0 | 0, 53  | 0) | 0 | 0, 52  | 0) | 0 | 0, 51  | 0) | 0 | 0, 50  | 0) | 0 | 0, 49  | 0) | 0 | 0, 48  | 0) | 0 | 0, 47  | 0) | 0 | 0, 46  | 0) | 0 | 0, 45  | 0) | 0 | 0, 44  | 0) | 0 | 0, 43  | 0) | 0 | 0, 42  | 0) | 0 | 0, 41  | 0) | 0 | 0, 40  | 0) | 0 | 0, 39  | 0) | 0 | 0, 38  | 0) | 0 | 0, 37  | 0) | 0 | 0, 36  | 0) | 0 | 0, 35  | 0) | 0 | 0, 34  | 0) | 0 | 0, 33  | 0) | 0 | 0, 32  | 0) | 0 | 0, 31  | 0) | 0 | 0, 30  | 0) | 0 | 0, 29  | 0) | 0 | 0, 28  | 0) | 0 | 0, 27  | 0) | 0 | 0, 26  | 0) | 0 | 0, 25  | 0) | 0 | 0, 24  | 0) | 0 | 0, 23  | 0) | 0 | 0, 22  | 0) | 0 | 0, 21  | 0) | 0 | 0, 20  | 0) | 0 | 0, 19  | 0) | 0 | 0, 18  | 0) | 0 | 0, 17  | 0) | 0 | 0, 16  | 0) | 0 | 0, 15  | 0) | 0 | 0, 14  | 0) | 0 | 0, 13  | 0) | 0 | 0, 12  | 0) | 0 | 0, 11  | 0) | 0 | 0, 10  | 0) | 0 | 0, 9  | 0) | 0 | 0, 8  | 0) | 0 | 0, 7  | 0) | 0 | 0, 6  | 0) | 0 | 0, 5  | 0) | 0 | 0, 4  | 0) | 0 | 0, 3  | 0) | 0 | 0, 2  | 0) | 0 | 0, 1  | 0) | 0 | 0);

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