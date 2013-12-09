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
    var c0_var_x0 = 0;
    var c0_var_x1 = 0;
    var c0_var_x10 = 0;
    var c0_var_x100 = 0;
    var c0_var_x101 = 0;
    var c0_var_x102 = 0;
    var c0_var_x103 = 0;
    var c0_var_x104 = 0;
    var c0_var_x105 = 0;
    var c0_var_x106 = 0;
    var c0_var_x107 = 0;
    var c0_var_x108 = 0;
    var c0_var_x109 = 0;
    var c0_var_x11 = 0;
    var c0_var_x110 = 0;
    var c0_var_x111 = 0;
    var c0_var_x112 = 0;
    var c0_var_x113 = 0;
    var c0_var_x114 = 0;
    var c0_var_x115 = 0;
    var c0_var_x116 = 0;
    var c0_var_x117 = 0;
    var c0_var_x118 = 0;
    var c0_var_x119 = 0;
    var c0_var_x12 = 0;
    var c0_var_x120 = 0;
    var c0_var_x121 = 0;
    var c0_var_x122 = 0;
    var c0_var_x123 = 0;
    var c0_var_x124 = 0;
    var c0_var_x125 = 0;
    var c0_var_x126 = 0;
    var c0_var_x127 = 0;
    var c0_var_x128 = 0;
    var c0_var_x129 = 0;
    var c0_var_x13 = 0;
    var c0_var_x130 = 0;
    var c0_var_x131 = 0;
    var c0_var_x132 = 0;
    var c0_var_x133 = 0;
    var c0_var_x134 = 0;
    var c0_var_x135 = 0;
    var c0_var_x136 = 0;
    var c0_var_x137 = 0;
    var c0_var_x138 = 0;
    var c0_var_x139 = 0;
    var c0_var_x14 = 0;
    var c0_var_x140 = 0;
    var c0_var_x141 = 0;
    var c0_var_x142 = 0;
    var c0_var_x143 = 0;
    var c0_var_x144 = 0;
    var c0_var_x145 = 0;
    var c0_var_x146 = 0;
    var c0_var_x147 = 0;
    var c0_var_x148 = 0;
    var c0_var_x149 = 0;
    var c0_var_x15 = 0;
    var c0_var_x150 = 0;
    var c0_var_x151 = 0;
    var c0_var_x152 = 0;
    var c0_var_x153 = 0;
    var c0_var_x154 = 0;
    var c0_var_x155 = 0;
    var c0_var_x156 = 0;
    var c0_var_x157 = 0;
    var c0_var_x158 = 0;
    var c0_var_x159 = 0;
    var c0_var_x16 = 0;
    var c0_var_x160 = 0;
    var c0_var_x161 = 0;
    var c0_var_x162 = 0;
    var c0_var_x163 = 0;
    var c0_var_x164 = 0;
    var c0_var_x165 = 0;
    var c0_var_x166 = 0;
    var c0_var_x167 = 0;
    var c0_var_x168 = 0;
    var c0_var_x169 = 0;
    var c0_var_x17 = 0;
    var c0_var_x170 = 0;
    var c0_var_x171 = 0;
    var c0_var_x172 = 0;
    var c0_var_x173 = 0;
    var c0_var_x174 = 0;
    var c0_var_x175 = 0;
    var c0_var_x176 = 0;
    var c0_var_x177 = 0;
    var c0_var_x178 = 0;
    var c0_var_x179 = 0;
    var c0_var_x18 = 0;
    var c0_var_x180 = 0;
    var c0_var_x181 = 0;
    var c0_var_x182 = 0;
    var c0_var_x183 = 0;
    var c0_var_x184 = 0;
    var c0_var_x185 = 0;
    var c0_var_x186 = 0;
    var c0_var_x187 = 0;
    var c0_var_x188 = 0;
    var c0_var_x189 = 0;
    var c0_var_x19 = 0;
    var c0_var_x190 = 0;
    var c0_var_x191 = 0;
    var c0_var_x192 = 0;
    var c0_var_x193 = 0;
    var c0_var_x194 = 0;
    var c0_var_x195 = 0;
    var c0_var_x196 = 0;
    var c0_var_x197 = 0;
    var c0_var_x198 = 0;
    var c0_var_x199 = 0;
    var c0_var_x2 = 0;
    var c0_var_x20 = 0;
    var c0_var_x21 = 0;
    var c0_var_x22 = 0;
    var c0_var_x23 = 0;
    var c0_var_x24 = 0;
    var c0_var_x25 = 0;
    var c0_var_x26 = 0;
    var c0_var_x27 = 0;
    var c0_var_x28 = 0;
    var c0_var_x29 = 0;
    var c0_var_x3 = 0;
    var c0_var_x30 = 0;
    var c0_var_x31 = 0;
    var c0_var_x32 = 0;
    var c0_var_x33 = 0;
    var c0_var_x34 = 0;
    var c0_var_x35 = 0;
    var c0_var_x36 = 0;
    var c0_var_x37 = 0;
    var c0_var_x38 = 0;
    var c0_var_x39 = 0;
    var c0_var_x4 = 0;
    var c0_var_x40 = 0;
    var c0_var_x41 = 0;
    var c0_var_x42 = 0;
    var c0_var_x43 = 0;
    var c0_var_x44 = 0;
    var c0_var_x45 = 0;
    var c0_var_x46 = 0;
    var c0_var_x47 = 0;
    var c0_var_x48 = 0;
    var c0_var_x49 = 0;
    var c0_var_x5 = 0;
    var c0_var_x50 = 0;
    var c0_var_x51 = 0;
    var c0_var_x52 = 0;
    var c0_var_x53 = 0;
    var c0_var_x54 = 0;
    var c0_var_x55 = 0;
    var c0_var_x56 = 0;
    var c0_var_x57 = 0;
    var c0_var_x58 = 0;
    var c0_var_x59 = 0;
    var c0_var_x6 = 0;
    var c0_var_x60 = 0;
    var c0_var_x61 = 0;
    var c0_var_x62 = 0;
    var c0_var_x63 = 0;
    var c0_var_x64 = 0;
    var c0_var_x65 = 0;
    var c0_var_x66 = 0;
    var c0_var_x67 = 0;
    var c0_var_x68 = 0;
    var c0_var_x69 = 0;
    var c0_var_x7 = 0;
    var c0_var_x70 = 0;
    var c0_var_x71 = 0;
    var c0_var_x72 = 0;
    var c0_var_x73 = 0;
    var c0_var_x74 = 0;
    var c0_var_x75 = 0;
    var c0_var_x76 = 0;
    var c0_var_x77 = 0;
    var c0_var_x78 = 0;
    var c0_var_x79 = 0;
    var c0_var_x8 = 0;
    var c0_var_x80 = 0;
    var c0_var_x81 = 0;
    var c0_var_x82 = 0;
    var c0_var_x83 = 0;
    var c0_var_x84 = 0;
    var c0_var_x85 = 0;
    var c0_var_x86 = 0;
    var c0_var_x87 = 0;
    var c0_var_x88 = 0;
    var c0_var_x89 = 0;
    var c0_var_x9 = 0;
    var c0_var_x90 = 0;
    var c0_var_x91 = 0;
    var c0_var_x92 = 0;
    var c0_var_x93 = 0;
    var c0_var_x94 = 0;
    var c0_var_x95 = 0;
    var c0_var_x96 = 0;
    var c0_var_x97 = 0;
    var c0_var_x98 = 0;
    var c0_var_x99 = 0;
     c0_var_x0  =  0 | 0;
     c0_var_x1  = ( 0  | 0) + ( 1  | 0)| 0;
     c0_var_x2  = ( c0_var_x1  | 0) + ( 1  | 0)| 0;
     c0_var_x3  = ( c0_var_x2  | 0) + ( 1  | 0)| 0;
     c0_var_x4  = ( c0_var_x3  | 0) + ( 1  | 0)| 0;
     c0_var_x5  = ( c0_var_x4  | 0) + ( 1  | 0)| 0;
     c0_var_x6  = ( c0_var_x5  | 0) + ( 1  | 0)| 0;
     c0_var_x7  = ( c0_var_x6  | 0) + ( 1  | 0)| 0;
     c0_var_x8  = ( c0_var_x7  | 0) + ( 1  | 0)| 0;
     c0_var_x9  = ( c0_var_x8  | 0) + ( 1  | 0)| 0;
     c0_var_x10  = ( c0_var_x9  | 0) + ( 1  | 0)| 0;
     c0_var_x11  = ( c0_var_x10  | 0) + ( 1  | 0)| 0;
     c0_var_x12  = ( c0_var_x11  | 0) + ( 1  | 0)| 0;
     c0_var_x13  = ( c0_var_x12  | 0) + ( 1  | 0)| 0;
     c0_var_x14  = ( c0_var_x13  | 0) + ( 1  | 0)| 0;
     c0_var_x15  = ( c0_var_x14  | 0) + ( 1  | 0)| 0;
     c0_var_x16  = ( c0_var_x15  | 0) + ( 1  | 0)| 0;
     c0_var_x17  = ( c0_var_x16  | 0) + ( 1  | 0)| 0;
     c0_var_x18  = ( c0_var_x17  | 0) + ( 1  | 0)| 0;
     c0_var_x19  = ( c0_var_x18  | 0) + ( 1  | 0)| 0;
     c0_var_x20  = ( c0_var_x19  | 0) + ( 1  | 0)| 0;
     c0_var_x21  = ( c0_var_x20  | 0) + ( 1  | 0)| 0;
     c0_var_x22  = ( c0_var_x21  | 0) + ( 1  | 0)| 0;
     c0_var_x23  = ( c0_var_x22  | 0) + ( 1  | 0)| 0;
     c0_var_x24  = ( c0_var_x23  | 0) + ( 1  | 0)| 0;
     c0_var_x25  = ( c0_var_x24  | 0) + ( 1  | 0)| 0;
     c0_var_x26  = ( c0_var_x25  | 0) + ( 1  | 0)| 0;
     c0_var_x27  = ( c0_var_x26  | 0) + ( 1  | 0)| 0;
     c0_var_x28  = ( c0_var_x27  | 0) + ( 1  | 0)| 0;
     c0_var_x29  = ( c0_var_x28  | 0) + ( 1  | 0)| 0;
     c0_var_x30  = ( c0_var_x29  | 0) + ( 1  | 0)| 0;
     c0_var_x31  = ( c0_var_x30  | 0) + ( 1  | 0)| 0;
     c0_var_x32  = ( c0_var_x31  | 0) + ( 1  | 0)| 0;
     c0_var_x33  = ( c0_var_x32  | 0) + ( 1  | 0)| 0;
     c0_var_x34  = ( c0_var_x33  | 0) + ( 1  | 0)| 0;
     c0_var_x35  = ( c0_var_x34  | 0) + ( 1  | 0)| 0;
     c0_var_x36  = ( c0_var_x35  | 0) + ( 1  | 0)| 0;
     c0_var_x37  = ( c0_var_x36  | 0) + ( 1  | 0)| 0;
     c0_var_x38  = ( c0_var_x37  | 0) + ( 1  | 0)| 0;
     c0_var_x39  = ( c0_var_x38  | 0) + ( 1  | 0)| 0;
     c0_var_x40  = ( c0_var_x39  | 0) + ( 1  | 0)| 0;
     c0_var_x41  = ( c0_var_x40  | 0) + ( 1  | 0)| 0;
     c0_var_x42  = ( c0_var_x41  | 0) + ( 1  | 0)| 0;
     c0_var_x43  = ( c0_var_x42  | 0) + ( 1  | 0)| 0;
     c0_var_x44  = ( c0_var_x43  | 0) + ( 1  | 0)| 0;
     c0_var_x45  = ( c0_var_x44  | 0) + ( 1  | 0)| 0;
     c0_var_x46  = ( c0_var_x45  | 0) + ( 1  | 0)| 0;
     c0_var_x47  = ( c0_var_x46  | 0) + ( 1  | 0)| 0;
     c0_var_x48  = ( c0_var_x47  | 0) + ( 1  | 0)| 0;
     c0_var_x49  = ( c0_var_x48  | 0) + ( 1  | 0)| 0;
     c0_var_x50  = ( c0_var_x49  | 0) + ( 1  | 0)| 0;
     c0_var_x51  = ( c0_var_x50  | 0) + ( 1  | 0)| 0;
     c0_var_x52  = ( c0_var_x51  | 0) + ( 1  | 0)| 0;
     c0_var_x53  = ( c0_var_x52  | 0) + ( 1  | 0)| 0;
     c0_var_x54  = ( c0_var_x53  | 0) + ( 1  | 0)| 0;
     c0_var_x55  = ( c0_var_x54  | 0) + ( 1  | 0)| 0;
     c0_var_x56  = ( c0_var_x55  | 0) + ( 1  | 0)| 0;
     c0_var_x57  = ( c0_var_x56  | 0) + ( 1  | 0)| 0;
     c0_var_x58  = ( c0_var_x57  | 0) + ( 1  | 0)| 0;
     c0_var_x59  = ( c0_var_x58  | 0) + ( 1  | 0)| 0;
     c0_var_x60  = ( c0_var_x59  | 0) + ( 1  | 0)| 0;
     c0_var_x61  = ( c0_var_x60  | 0) + ( 1  | 0)| 0;
     c0_var_x62  = ( c0_var_x61  | 0) + ( 1  | 0)| 0;
     c0_var_x63  = ( c0_var_x62  | 0) + ( 1  | 0)| 0;
     c0_var_x64  = ( c0_var_x63  | 0) + ( 1  | 0)| 0;
     c0_var_x65  = ( c0_var_x64  | 0) + ( 1  | 0)| 0;
     c0_var_x66  = ( c0_var_x65  | 0) + ( 1  | 0)| 0;
     c0_var_x67  = ( c0_var_x66  | 0) + ( 1  | 0)| 0;
     c0_var_x68  = ( c0_var_x67  | 0) + ( 1  | 0)| 0;
     c0_var_x69  = ( c0_var_x68  | 0) + ( 1  | 0)| 0;
     c0_var_x70  = ( c0_var_x69  | 0) + ( 1  | 0)| 0;
     c0_var_x71  = ( c0_var_x70  | 0) + ( 1  | 0)| 0;
     c0_var_x72  = ( c0_var_x71  | 0) + ( 1  | 0)| 0;
     c0_var_x73  = ( c0_var_x72  | 0) + ( 1  | 0)| 0;
     c0_var_x74  = ( c0_var_x73  | 0) + ( 1  | 0)| 0;
     c0_var_x75  = ( c0_var_x74  | 0) + ( 1  | 0)| 0;
     c0_var_x76  = ( c0_var_x75  | 0) + ( 1  | 0)| 0;
     c0_var_x77  = ( c0_var_x76  | 0) + ( 1  | 0)| 0;
     c0_var_x78  = ( c0_var_x77  | 0) + ( 1  | 0)| 0;
     c0_var_x79  = ( c0_var_x78  | 0) + ( 1  | 0)| 0;
     c0_var_x80  = ( c0_var_x79  | 0) + ( 1  | 0)| 0;
     c0_var_x81  = ( c0_var_x80  | 0) + ( 1  | 0)| 0;
     c0_var_x82  = ( c0_var_x81  | 0) + ( 1  | 0)| 0;
     c0_var_x83  = ( c0_var_x82  | 0) + ( 1  | 0)| 0;
     c0_var_x84  = ( c0_var_x83  | 0) + ( 1  | 0)| 0;
     c0_var_x85  = ( c0_var_x84  | 0) + ( 1  | 0)| 0;
     c0_var_x86  = ( c0_var_x85  | 0) + ( 1  | 0)| 0;
     c0_var_x87  = ( c0_var_x86  | 0) + ( 1  | 0)| 0;
     c0_var_x88  = ( c0_var_x87  | 0) + ( 1  | 0)| 0;
     c0_var_x89  = ( c0_var_x88  | 0) + ( 1  | 0)| 0;
     c0_var_x90  = ( c0_var_x89  | 0) + ( 1  | 0)| 0;
     c0_var_x91  = ( c0_var_x90  | 0) + ( 1  | 0)| 0;
     c0_var_x92  = ( c0_var_x91  | 0) + ( 1  | 0)| 0;
     c0_var_x93  = ( c0_var_x92  | 0) + ( 1  | 0)| 0;
     c0_var_x94  = ( c0_var_x93  | 0) + ( 1  | 0)| 0;
     c0_var_x95  = ( c0_var_x94  | 0) + ( 1  | 0)| 0;
     c0_var_x96  = ( c0_var_x95  | 0) + ( 1  | 0)| 0;
     c0_var_x97  = ( c0_var_x96  | 0) + ( 1  | 0)| 0;
     c0_var_x98  = ( c0_var_x97  | 0) + ( 1  | 0)| 0;
     c0_var_x99  = ( c0_var_x98  | 0) + ( 1  | 0)| 0;
     c0_var_x100  = ( c0_var_x99  | 0) + ( 1  | 0)| 0;
     c0_var_x101  = ( c0_var_x100  | 0) + ( 1  | 0)| 0;
     c0_var_x102  = ( c0_var_x101  | 0) + ( 1  | 0)| 0;
     c0_var_x103  = ( c0_var_x102  | 0) + ( 1  | 0)| 0;
     c0_var_x104  = ( c0_var_x103  | 0) + ( 1  | 0)| 0;
     c0_var_x105  = ( c0_var_x104  | 0) + ( 1  | 0)| 0;
     c0_var_x106  = ( c0_var_x105  | 0) + ( 1  | 0)| 0;
     c0_var_x107  = ( c0_var_x106  | 0) + ( 1  | 0)| 0;
     c0_var_x108  = ( c0_var_x107  | 0) + ( 1  | 0)| 0;
     c0_var_x109  = ( c0_var_x108  | 0) + ( 1  | 0)| 0;
     c0_var_x110  = ( c0_var_x109  | 0) + ( 1  | 0)| 0;
     c0_var_x111  = ( c0_var_x110  | 0) + ( 1  | 0)| 0;
     c0_var_x112  = ( c0_var_x111  | 0) + ( 1  | 0)| 0;
     c0_var_x113  = ( c0_var_x112  | 0) + ( 1  | 0)| 0;
     c0_var_x114  = ( c0_var_x113  | 0) + ( 1  | 0)| 0;
     c0_var_x115  = ( c0_var_x114  | 0) + ( 1  | 0)| 0;
     c0_var_x116  = ( c0_var_x115  | 0) + ( 1  | 0)| 0;
     c0_var_x117  = ( c0_var_x116  | 0) + ( 1  | 0)| 0;
     c0_var_x118  = ( c0_var_x117  | 0) + ( 1  | 0)| 0;
     c0_var_x119  = ( c0_var_x118  | 0) + ( 1  | 0)| 0;
     c0_var_x120  = ( c0_var_x119  | 0) + ( 1  | 0)| 0;
     c0_var_x121  = ( c0_var_x120  | 0) + ( 1  | 0)| 0;
     c0_var_x122  = ( c0_var_x121  | 0) + ( 1  | 0)| 0;
     c0_var_x123  = ( c0_var_x122  | 0) + ( 1  | 0)| 0;
     c0_var_x124  = ( c0_var_x123  | 0) + ( 1  | 0)| 0;
     c0_var_x125  = ( c0_var_x124  | 0) + ( 1  | 0)| 0;
     c0_var_x126  = ( c0_var_x125  | 0) + ( 1  | 0)| 0;
     c0_var_x127  = ( c0_var_x126  | 0) + ( 1  | 0)| 0;
     c0_var_x128  = ( c0_var_x127  | 0) + ( 1  | 0)| 0;
     c0_var_x129  = ( c0_var_x128  | 0) + ( 1  | 0)| 0;
     c0_var_x130  = ( c0_var_x129  | 0) + ( 1  | 0)| 0;
     c0_var_x131  = ( c0_var_x130  | 0) + ( 1  | 0)| 0;
     c0_var_x132  = ( c0_var_x131  | 0) + ( 1  | 0)| 0;
     c0_var_x133  = ( c0_var_x132  | 0) + ( 1  | 0)| 0;
     c0_var_x134  = ( c0_var_x133  | 0) + ( 1  | 0)| 0;
     c0_var_x135  = ( c0_var_x134  | 0) + ( 1  | 0)| 0;
     c0_var_x136  = ( c0_var_x135  | 0) + ( 1  | 0)| 0;
     c0_var_x137  = ( c0_var_x136  | 0) + ( 1  | 0)| 0;
     c0_var_x138  = ( c0_var_x137  | 0) + ( 1  | 0)| 0;
     c0_var_x139  = ( c0_var_x138  | 0) + ( 1  | 0)| 0;
     c0_var_x140  = ( c0_var_x139  | 0) + ( 1  | 0)| 0;
     c0_var_x141  = ( c0_var_x140  | 0) + ( 1  | 0)| 0;
     c0_var_x142  = ( c0_var_x141  | 0) + ( 1  | 0)| 0;
     c0_var_x143  = ( c0_var_x142  | 0) + ( 1  | 0)| 0;
     c0_var_x144  = ( c0_var_x143  | 0) + ( 1  | 0)| 0;
     c0_var_x145  = ( c0_var_x144  | 0) + ( 1  | 0)| 0;
     c0_var_x146  = ( c0_var_x145  | 0) + ( 1  | 0)| 0;
     c0_var_x147  = ( c0_var_x146  | 0) + ( 1  | 0)| 0;
     c0_var_x148  = ( c0_var_x147  | 0) + ( 1  | 0)| 0;
     c0_var_x149  = ( c0_var_x148  | 0) + ( 1  | 0)| 0;
     c0_var_x150  = ( c0_var_x149  | 0) + ( 1  | 0)| 0;
     c0_var_x151  = ( c0_var_x150  | 0) + ( 1  | 0)| 0;
     c0_var_x152  = ( c0_var_x151  | 0) + ( 1  | 0)| 0;
     c0_var_x153  = ( c0_var_x152  | 0) + ( 1  | 0)| 0;
     c0_var_x154  = ( c0_var_x153  | 0) + ( 1  | 0)| 0;
     c0_var_x155  = ( c0_var_x154  | 0) + ( 1  | 0)| 0;
     c0_var_x156  = ( c0_var_x155  | 0) + ( 1  | 0)| 0;
     c0_var_x157  = ( c0_var_x156  | 0) + ( 1  | 0)| 0;
     c0_var_x158  = ( c0_var_x157  | 0) + ( 1  | 0)| 0;
     c0_var_x159  = ( c0_var_x158  | 0) + ( 1  | 0)| 0;
     c0_var_x160  = ( c0_var_x159  | 0) + ( 1  | 0)| 0;
     c0_var_x161  = ( c0_var_x160  | 0) + ( 1  | 0)| 0;
     c0_var_x162  = ( c0_var_x161  | 0) + ( 1  | 0)| 0;
     c0_var_x163  = ( c0_var_x162  | 0) + ( 1  | 0)| 0;
     c0_var_x164  = ( c0_var_x163  | 0) + ( 1  | 0)| 0;
     c0_var_x165  = ( c0_var_x164  | 0) + ( 1  | 0)| 0;
     c0_var_x166  = ( c0_var_x165  | 0) + ( 1  | 0)| 0;
     c0_var_x167  = ( c0_var_x166  | 0) + ( 1  | 0)| 0;
     c0_var_x168  = ( c0_var_x167  | 0) + ( 1  | 0)| 0;
     c0_var_x169  = ( c0_var_x168  | 0) + ( 1  | 0)| 0;
     c0_var_x170  = ( c0_var_x169  | 0) + ( 1  | 0)| 0;
     c0_var_x171  = ( c0_var_x170  | 0) + ( 1  | 0)| 0;
     c0_var_x172  = ( c0_var_x171  | 0) + ( 1  | 0)| 0;
     c0_var_x173  = ( c0_var_x172  | 0) + ( 1  | 0)| 0;
     c0_var_x174  = ( c0_var_x173  | 0) + ( 1  | 0)| 0;
     c0_var_x175  = ( c0_var_x174  | 0) + ( 1  | 0)| 0;
     c0_var_x176  = ( c0_var_x175  | 0) + ( 1  | 0)| 0;
     c0_var_x177  = ( c0_var_x176  | 0) + ( 1  | 0)| 0;
     c0_var_x178  = ( c0_var_x177  | 0) + ( 1  | 0)| 0;
     c0_var_x179  = ( c0_var_x178  | 0) + ( 1  | 0)| 0;
     c0_var_x180  = ( c0_var_x179  | 0) + ( 1  | 0)| 0;
     c0_var_x181  = ( c0_var_x180  | 0) + ( 1  | 0)| 0;
     c0_var_x182  = ( c0_var_x181  | 0) + ( 1  | 0)| 0;
     c0_var_x183  = ( c0_var_x182  | 0) + ( 1  | 0)| 0;
     c0_var_x184  = ( c0_var_x183  | 0) + ( 1  | 0)| 0;
     c0_var_x185  = ( c0_var_x184  | 0) + ( 1  | 0)| 0;
     c0_var_x186  = ( c0_var_x185  | 0) + ( 1  | 0)| 0;
     c0_var_x187  = ( c0_var_x186  | 0) + ( 1  | 0)| 0;
     c0_var_x188  = ( c0_var_x187  | 0) + ( 1  | 0)| 0;
     c0_var_x189  = ( c0_var_x188  | 0) + ( 1  | 0)| 0;
     c0_var_x190  = ( c0_var_x189  | 0) + ( 1  | 0)| 0;
     c0_var_x191  = ( c0_var_x190  | 0) + ( 1  | 0)| 0;
     c0_var_x192  = ( c0_var_x191  | 0) + ( 1  | 0)| 0;
     c0_var_x193  = ( c0_var_x192  | 0) + ( 1  | 0)| 0;
     c0_var_x194  = ( c0_var_x193  | 0) + ( 1  | 0)| 0;
     c0_var_x195  = ( c0_var_x194  | 0) + ( 1  | 0)| 0;
     c0_var_x196  = ( c0_var_x195  | 0) + ( 1  | 0)| 0;
     c0_var_x197  = ( c0_var_x196  | 0) + ( 1  | 0)| 0;
     c0_var_x198  = ( c0_var_x197  | 0) + ( 1  | 0)| 0;
     c0_var_x199  = ( c0_var_x198  | 0) + ( 1  | 0)| 0;
    return (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((( 0  | 0) + ( c0_var_x1  | 0) | 0) + ( c0_var_x2  | 0) | 0) + ( c0_var_x3  | 0) | 0) + ( c0_var_x4  | 0) | 0) + ( c0_var_x5  | 0) | 0) + ( c0_var_x6  | 0) | 0) + ( c0_var_x7  | 0) | 0) + ( c0_var_x8  | 0) | 0) + ( c0_var_x9  | 0) | 0) + ( c0_var_x10  | 0) | 0) + ( c0_var_x11  | 0) | 0) + ( c0_var_x12  | 0) | 0) + ( c0_var_x13  | 0) | 0) + ( c0_var_x14  | 0) | 0) + ( c0_var_x15  | 0) | 0) + ( c0_var_x16  | 0) | 0) + ( c0_var_x17  | 0) | 0) + ( c0_var_x18  | 0) | 0) + ( c0_var_x19  | 0) | 0) + ( c0_var_x20  | 0) | 0) + ( c0_var_x21  | 0) | 0) + ( c0_var_x22  | 0) | 0) + ( c0_var_x23  | 0) | 0) + ( c0_var_x24  | 0) | 0) + ( c0_var_x25  | 0) | 0) + ( c0_var_x26  | 0) | 0) + ( c0_var_x27  | 0) | 0) + ( c0_var_x28  | 0) | 0) + ( c0_var_x29  | 0) | 0) + ( c0_var_x30  | 0) | 0) + ( c0_var_x31  | 0) | 0) + ( c0_var_x32  | 0) | 0) + ( c0_var_x33  | 0) | 0) + ( c0_var_x34  | 0) | 0) + ( c0_var_x35  | 0) | 0) + ( c0_var_x36  | 0) | 0) + ( c0_var_x37  | 0) | 0) + ( c0_var_x38  | 0) | 0) + ( c0_var_x39  | 0) | 0) + ( c0_var_x40  | 0) | 0) + ( c0_var_x41  | 0) | 0) + ( c0_var_x42  | 0) | 0) + ( c0_var_x43  | 0) | 0) + ( c0_var_x44  | 0) | 0) + ( c0_var_x45  | 0) | 0) + ( c0_var_x46  | 0) | 0) + ( c0_var_x47  | 0) | 0) + ( c0_var_x48  | 0) | 0) + ( c0_var_x49  | 0) | 0) + ( c0_var_x50  | 0) | 0) + ( c0_var_x51  | 0) | 0) + ( c0_var_x52  | 0) | 0) + ( c0_var_x53  | 0) | 0) + ( c0_var_x54  | 0) | 0) + ( c0_var_x55  | 0) | 0) + ( c0_var_x56  | 0) | 0) + ( c0_var_x57  | 0) | 0) + ( c0_var_x58  | 0) | 0) + ( c0_var_x59  | 0) | 0) + ( c0_var_x60  | 0) | 0) + ( c0_var_x61  | 0) | 0) + ( c0_var_x62  | 0) | 0) + ( c0_var_x63  | 0) | 0) + ( c0_var_x64  | 0) | 0) + ( c0_var_x65  | 0) | 0) + ( c0_var_x66  | 0) | 0) + ( c0_var_x67  | 0) | 0) + ( c0_var_x68  | 0) | 0) + ( c0_var_x69  | 0) | 0) + ( c0_var_x70  | 0) | 0) + ( c0_var_x71  | 0) | 0) + ( c0_var_x72  | 0) | 0) + ( c0_var_x73  | 0) | 0) + ( c0_var_x74  | 0) | 0) + ( c0_var_x75  | 0) | 0) + ( c0_var_x76  | 0) | 0) + ( c0_var_x77  | 0) | 0) + ( c0_var_x78  | 0) | 0) + ( c0_var_x79  | 0) | 0) + ( c0_var_x80  | 0) | 0) + ( c0_var_x81  | 0) | 0) + ( c0_var_x82  | 0) | 0) + ( c0_var_x83  | 0) | 0) + ( c0_var_x84  | 0) | 0) + ( c0_var_x85  | 0) | 0) + ( c0_var_x86  | 0) | 0) + ( c0_var_x87  | 0) | 0) + ( c0_var_x88  | 0) | 0) + ( c0_var_x89  | 0) | 0) + ( c0_var_x90  | 0) | 0) + ( c0_var_x91  | 0) | 0) + ( c0_var_x92  | 0) | 0) + ( c0_var_x93  | 0) | 0) + ( c0_var_x94  | 0) | 0) + ( c0_var_x95  | 0) | 0) + ( c0_var_x96  | 0) | 0) + ( c0_var_x97  | 0) | 0) + ( c0_var_x98  | 0) | 0) + ( c0_var_x99  | 0) | 0) + ( c0_var_x100  | 0) | 0) + ( c0_var_x101  | 0) | 0) + ( c0_var_x102  | 0) | 0) + ( c0_var_x103  | 0) | 0) + ( c0_var_x104  | 0) | 0) + ( c0_var_x105  | 0) | 0) + ( c0_var_x106  | 0) | 0) + ( c0_var_x107  | 0) | 0) + ( c0_var_x108  | 0) | 0) + ( c0_var_x109  | 0) | 0) + ( c0_var_x110  | 0) | 0) + ( c0_var_x111  | 0) | 0) + ( c0_var_x112  | 0) | 0) + ( c0_var_x113  | 0) | 0) + ( c0_var_x114  | 0) | 0) + ( c0_var_x115  | 0) | 0) + ( c0_var_x116  | 0) | 0) + ( c0_var_x117  | 0) | 0) + ( c0_var_x118  | 0) | 0) + ( c0_var_x119  | 0) | 0) + ( c0_var_x120  | 0) | 0) + ( c0_var_x121  | 0) | 0) + ( c0_var_x122  | 0) | 0) + ( c0_var_x123  | 0) | 0) + ( c0_var_x124  | 0) | 0) + ( c0_var_x125  | 0) | 0) + ( c0_var_x126  | 0) | 0) + ( c0_var_x127  | 0) | 0) + ( c0_var_x128  | 0) | 0) + ( c0_var_x129  | 0) | 0) + ( c0_var_x130  | 0) | 0) + ( c0_var_x131  | 0) | 0) + ( c0_var_x132  | 0) | 0) + ( c0_var_x133  | 0) | 0) + ( c0_var_x134  | 0) | 0) + ( c0_var_x135  | 0) | 0) + ( c0_var_x136  | 0) | 0) + ( c0_var_x137  | 0) | 0) + ( c0_var_x138  | 0) | 0) + ( c0_var_x139  | 0) | 0) + ( c0_var_x140  | 0) | 0) + ( c0_var_x141  | 0) | 0) + ( c0_var_x142  | 0) | 0) + ( c0_var_x143  | 0) | 0) + ( c0_var_x144  | 0) | 0) + ( c0_var_x145  | 0) | 0) + ( c0_var_x146  | 0) | 0) + ( c0_var_x147  | 0) | 0) + ( c0_var_x148  | 0) | 0) + ( c0_var_x149  | 0) | 0) + ( c0_var_x150  | 0) | 0) + ( c0_var_x151  | 0) | 0) + ( c0_var_x152  | 0) | 0) + ( c0_var_x153  | 0) | 0) + ( c0_var_x154  | 0) | 0) + ( c0_var_x155  | 0) | 0) + ( c0_var_x156  | 0) | 0) + ( c0_var_x157  | 0) | 0) + ( c0_var_x158  | 0) | 0) + ( c0_var_x159  | 0) | 0) + ( c0_var_x160  | 0) | 0) + ( c0_var_x161  | 0) | 0) + ( c0_var_x162  | 0) | 0) + ( c0_var_x163  | 0) | 0) + ( c0_var_x164  | 0) | 0) + ( c0_var_x165  | 0) | 0) + ( c0_var_x166  | 0) | 0) + ( c0_var_x167  | 0) | 0) + ( c0_var_x168  | 0) | 0) + ( c0_var_x169  | 0) | 0) + ( c0_var_x170  | 0) | 0) + ( c0_var_x171  | 0) | 0) + ( c0_var_x172  | 0) | 0) + ( c0_var_x173  | 0) | 0) + ( c0_var_x174  | 0) | 0) + ( c0_var_x175  | 0) | 0) + ( c0_var_x176  | 0) | 0) + ( c0_var_x177  | 0) | 0) + ( c0_var_x178  | 0) | 0) + ( c0_var_x179  | 0) | 0) + ( c0_var_x180  | 0) | 0) + ( c0_var_x181  | 0) | 0) + ( c0_var_x182  | 0) | 0) + ( c0_var_x183  | 0) | 0) + ( c0_var_x184  | 0) | 0) + ( c0_var_x185  | 0) | 0) + ( c0_var_x186  | 0) | 0) + ( c0_var_x187  | 0) | 0) + ( c0_var_x188  | 0) | 0) + ( c0_var_x189  | 0) | 0) + ( c0_var_x190  | 0) | 0) + ( c0_var_x191  | 0) | 0) + ( c0_var_x192  | 0) | 0) + ( c0_var_x193  | 0) | 0) + ( c0_var_x194  | 0) | 0) + ( c0_var_x195  | 0) | 0) + ( c0_var_x196  | 0) | 0) + ( c0_var_x197  | 0) | 0) + ( c0_var_x198  | 0) | 0) + ( c0_var_x199  | 0) | 0);

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