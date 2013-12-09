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
    var c0_var_var0 = 0;
    var c0_var_var1 = 0;
    var c0_var_var10 = 0;
    var c0_var_var100 = 0;
    var c0_var_var101 = 0;
    var c0_var_var102 = 0;
    var c0_var_var103 = 0;
    var c0_var_var104 = 0;
    var c0_var_var105 = 0;
    var c0_var_var106 = 0;
    var c0_var_var107 = 0;
    var c0_var_var108 = 0;
    var c0_var_var109 = 0;
    var c0_var_var11 = 0;
    var c0_var_var110 = 0;
    var c0_var_var111 = 0;
    var c0_var_var112 = 0;
    var c0_var_var113 = 0;
    var c0_var_var114 = 0;
    var c0_var_var115 = 0;
    var c0_var_var116 = 0;
    var c0_var_var117 = 0;
    var c0_var_var118 = 0;
    var c0_var_var119 = 0;
    var c0_var_var12 = 0;
    var c0_var_var120 = 0;
    var c0_var_var121 = 0;
    var c0_var_var122 = 0;
    var c0_var_var123 = 0;
    var c0_var_var124 = 0;
    var c0_var_var125 = 0;
    var c0_var_var126 = 0;
    var c0_var_var127 = 0;
    var c0_var_var128 = 0;
    var c0_var_var129 = 0;
    var c0_var_var13 = 0;
    var c0_var_var130 = 0;
    var c0_var_var131 = 0;
    var c0_var_var132 = 0;
    var c0_var_var133 = 0;
    var c0_var_var134 = 0;
    var c0_var_var135 = 0;
    var c0_var_var136 = 0;
    var c0_var_var137 = 0;
    var c0_var_var138 = 0;
    var c0_var_var139 = 0;
    var c0_var_var14 = 0;
    var c0_var_var140 = 0;
    var c0_var_var141 = 0;
    var c0_var_var142 = 0;
    var c0_var_var143 = 0;
    var c0_var_var144 = 0;
    var c0_var_var145 = 0;
    var c0_var_var146 = 0;
    var c0_var_var147 = 0;
    var c0_var_var148 = 0;
    var c0_var_var149 = 0;
    var c0_var_var15 = 0;
    var c0_var_var150 = 0;
    var c0_var_var151 = 0;
    var c0_var_var152 = 0;
    var c0_var_var153 = 0;
    var c0_var_var154 = 0;
    var c0_var_var155 = 0;
    var c0_var_var156 = 0;
    var c0_var_var157 = 0;
    var c0_var_var158 = 0;
    var c0_var_var159 = 0;
    var c0_var_var16 = 0;
    var c0_var_var160 = 0;
    var c0_var_var161 = 0;
    var c0_var_var162 = 0;
    var c0_var_var163 = 0;
    var c0_var_var164 = 0;
    var c0_var_var165 = 0;
    var c0_var_var166 = 0;
    var c0_var_var167 = 0;
    var c0_var_var168 = 0;
    var c0_var_var169 = 0;
    var c0_var_var17 = 0;
    var c0_var_var170 = 0;
    var c0_var_var171 = 0;
    var c0_var_var172 = 0;
    var c0_var_var173 = 0;
    var c0_var_var174 = 0;
    var c0_var_var175 = 0;
    var c0_var_var176 = 0;
    var c0_var_var177 = 0;
    var c0_var_var178 = 0;
    var c0_var_var179 = 0;
    var c0_var_var18 = 0;
    var c0_var_var180 = 0;
    var c0_var_var181 = 0;
    var c0_var_var182 = 0;
    var c0_var_var183 = 0;
    var c0_var_var184 = 0;
    var c0_var_var185 = 0;
    var c0_var_var186 = 0;
    var c0_var_var187 = 0;
    var c0_var_var188 = 0;
    var c0_var_var189 = 0;
    var c0_var_var19 = 0;
    var c0_var_var190 = 0;
    var c0_var_var191 = 0;
    var c0_var_var192 = 0;
    var c0_var_var193 = 0;
    var c0_var_var194 = 0;
    var c0_var_var195 = 0;
    var c0_var_var196 = 0;
    var c0_var_var197 = 0;
    var c0_var_var198 = 0;
    var c0_var_var199 = 0;
    var c0_var_var2 = 0;
    var c0_var_var20 = 0;
    var c0_var_var200 = 0;
    var c0_var_var201 = 0;
    var c0_var_var202 = 0;
    var c0_var_var203 = 0;
    var c0_var_var204 = 0;
    var c0_var_var205 = 0;
    var c0_var_var206 = 0;
    var c0_var_var207 = 0;
    var c0_var_var208 = 0;
    var c0_var_var209 = 0;
    var c0_var_var21 = 0;
    var c0_var_var210 = 0;
    var c0_var_var211 = 0;
    var c0_var_var212 = 0;
    var c0_var_var213 = 0;
    var c0_var_var214 = 0;
    var c0_var_var215 = 0;
    var c0_var_var216 = 0;
    var c0_var_var217 = 0;
    var c0_var_var218 = 0;
    var c0_var_var219 = 0;
    var c0_var_var22 = 0;
    var c0_var_var220 = 0;
    var c0_var_var221 = 0;
    var c0_var_var222 = 0;
    var c0_var_var223 = 0;
    var c0_var_var224 = 0;
    var c0_var_var225 = 0;
    var c0_var_var226 = 0;
    var c0_var_var227 = 0;
    var c0_var_var228 = 0;
    var c0_var_var229 = 0;
    var c0_var_var23 = 0;
    var c0_var_var230 = 0;
    var c0_var_var231 = 0;
    var c0_var_var232 = 0;
    var c0_var_var233 = 0;
    var c0_var_var234 = 0;
    var c0_var_var235 = 0;
    var c0_var_var236 = 0;
    var c0_var_var237 = 0;
    var c0_var_var238 = 0;
    var c0_var_var239 = 0;
    var c0_var_var24 = 0;
    var c0_var_var240 = 0;
    var c0_var_var241 = 0;
    var c0_var_var242 = 0;
    var c0_var_var243 = 0;
    var c0_var_var244 = 0;
    var c0_var_var245 = 0;
    var c0_var_var246 = 0;
    var c0_var_var247 = 0;
    var c0_var_var248 = 0;
    var c0_var_var249 = 0;
    var c0_var_var25 = 0;
    var c0_var_var250 = 0;
    var c0_var_var251 = 0;
    var c0_var_var252 = 0;
    var c0_var_var253 = 0;
    var c0_var_var254 = 0;
    var c0_var_var255 = 0;
    var c0_var_var26 = 0;
    var c0_var_var27 = 0;
    var c0_var_var28 = 0;
    var c0_var_var29 = 0;
    var c0_var_var3 = 0;
    var c0_var_var30 = 0;
    var c0_var_var31 = 0;
    var c0_var_var32 = 0;
    var c0_var_var33 = 0;
    var c0_var_var34 = 0;
    var c0_var_var35 = 0;
    var c0_var_var36 = 0;
    var c0_var_var37 = 0;
    var c0_var_var38 = 0;
    var c0_var_var39 = 0;
    var c0_var_var4 = 0;
    var c0_var_var40 = 0;
    var c0_var_var41 = 0;
    var c0_var_var42 = 0;
    var c0_var_var43 = 0;
    var c0_var_var44 = 0;
    var c0_var_var45 = 0;
    var c0_var_var46 = 0;
    var c0_var_var47 = 0;
    var c0_var_var48 = 0;
    var c0_var_var49 = 0;
    var c0_var_var5 = 0;
    var c0_var_var50 = 0;
    var c0_var_var51 = 0;
    var c0_var_var52 = 0;
    var c0_var_var53 = 0;
    var c0_var_var54 = 0;
    var c0_var_var55 = 0;
    var c0_var_var56 = 0;
    var c0_var_var57 = 0;
    var c0_var_var58 = 0;
    var c0_var_var59 = 0;
    var c0_var_var6 = 0;
    var c0_var_var60 = 0;
    var c0_var_var61 = 0;
    var c0_var_var62 = 0;
    var c0_var_var63 = 0;
    var c0_var_var64 = 0;
    var c0_var_var65 = 0;
    var c0_var_var66 = 0;
    var c0_var_var67 = 0;
    var c0_var_var68 = 0;
    var c0_var_var69 = 0;
    var c0_var_var7 = 0;
    var c0_var_var70 = 0;
    var c0_var_var71 = 0;
    var c0_var_var72 = 0;
    var c0_var_var73 = 0;
    var c0_var_var74 = 0;
    var c0_var_var75 = 0;
    var c0_var_var76 = 0;
    var c0_var_var77 = 0;
    var c0_var_var78 = 0;
    var c0_var_var79 = 0;
    var c0_var_var8 = 0;
    var c0_var_var80 = 0;
    var c0_var_var81 = 0;
    var c0_var_var82 = 0;
    var c0_var_var83 = 0;
    var c0_var_var84 = 0;
    var c0_var_var85 = 0;
    var c0_var_var86 = 0;
    var c0_var_var87 = 0;
    var c0_var_var88 = 0;
    var c0_var_var89 = 0;
    var c0_var_var9 = 0;
    var c0_var_var90 = 0;
    var c0_var_var91 = 0;
    var c0_var_var92 = 0;
    var c0_var_var93 = 0;
    var c0_var_var94 = 0;
    var c0_var_var95 = 0;
    var c0_var_var96 = 0;
    var c0_var_var97 = 0;
    var c0_var_var98 = 0;
    var c0_var_var99 = 0;
     c0_var_var0  =  0 | 0;
     c0_var_var1  =  1 | 0;
     c0_var_var2  = ( 1  | 0) + ( 0  | 0)| 0;
     c0_var_var3  = ( c0_var_var2  | 0) + ( 1  | 0)| 0;
     c0_var_var4  = ( c0_var_var3  | 0) + ( c0_var_var2  | 0)| 0;
     c0_var_var5  = ( c0_var_var4  | 0) + ( c0_var_var3  | 0)| 0;
     c0_var_var6  = ( c0_var_var5  | 0) + ( c0_var_var4  | 0)| 0;
     c0_var_var7  = ( c0_var_var6  | 0) + ( c0_var_var5  | 0)| 0;
     c0_var_var8  = ( c0_var_var7  | 0) + ( c0_var_var6  | 0)| 0;
     c0_var_var9  = ( c0_var_var8  | 0) + ( c0_var_var7  | 0)| 0;
     c0_var_var10  = ( c0_var_var9  | 0) + ( c0_var_var8  | 0)| 0;
     c0_var_var11  = ( c0_var_var10  | 0) + ( c0_var_var9  | 0)| 0;
     c0_var_var12  = ( c0_var_var11  | 0) + ( c0_var_var10  | 0)| 0;
     c0_var_var13  = ( c0_var_var12  | 0) + ( c0_var_var11  | 0)| 0;
     c0_var_var14  = ( c0_var_var13  | 0) + ( c0_var_var12  | 0)| 0;
     c0_var_var15  = ( c0_var_var14  | 0) + ( c0_var_var13  | 0)| 0;
     c0_var_var16  = ( c0_var_var15  | 0) + ( c0_var_var14  | 0)| 0;
     c0_var_var17  = ( c0_var_var16  | 0) + ( c0_var_var15  | 0)| 0;
     c0_var_var18  = ( c0_var_var17  | 0) + ( c0_var_var16  | 0)| 0;
     c0_var_var19  = ( c0_var_var18  | 0) + ( c0_var_var17  | 0)| 0;
     c0_var_var20  = ( c0_var_var19  | 0) + ( c0_var_var18  | 0)| 0;
     c0_var_var21  = ( c0_var_var20  | 0) + ( c0_var_var19  | 0)| 0;
     c0_var_var22  = ( c0_var_var21  | 0) + ( c0_var_var20  | 0)| 0;
     c0_var_var23  = ( c0_var_var22  | 0) + ( c0_var_var21  | 0)| 0;
     c0_var_var24  = ( c0_var_var23  | 0) + ( c0_var_var22  | 0)| 0;
     c0_var_var25  = ( c0_var_var24  | 0) + ( c0_var_var23  | 0)| 0;
     c0_var_var26  = ( c0_var_var25  | 0) + ( c0_var_var24  | 0)| 0;
     c0_var_var27  = ( c0_var_var26  | 0) + ( c0_var_var25  | 0)| 0;
     c0_var_var28  = ( c0_var_var27  | 0) + ( c0_var_var26  | 0)| 0;
     c0_var_var29  = ( c0_var_var28  | 0) + ( c0_var_var27  | 0)| 0;
     c0_var_var30  = ( c0_var_var29  | 0) + ( c0_var_var28  | 0)| 0;
     c0_var_var31  = ( c0_var_var30  | 0) + ( c0_var_var29  | 0)| 0;
     c0_var_var32  = ( c0_var_var31  | 0) + ( c0_var_var30  | 0)| 0;
     c0_var_var33  = ( c0_var_var32  | 0) + ( c0_var_var31  | 0)| 0;
     c0_var_var34  = ( c0_var_var33  | 0) + ( c0_var_var32  | 0)| 0;
     c0_var_var35  = ( c0_var_var34  | 0) + ( c0_var_var33  | 0)| 0;
     c0_var_var36  = ( c0_var_var35  | 0) + ( c0_var_var34  | 0)| 0;
     c0_var_var37  = ( c0_var_var36  | 0) + ( c0_var_var35  | 0)| 0;
     c0_var_var38  = ( c0_var_var37  | 0) + ( c0_var_var36  | 0)| 0;
     c0_var_var39  = ( c0_var_var38  | 0) + ( c0_var_var37  | 0)| 0;
     c0_var_var40  = ( c0_var_var39  | 0) + ( c0_var_var38  | 0)| 0;
     c0_var_var41  = ( c0_var_var40  | 0) + ( c0_var_var39  | 0)| 0;
     c0_var_var42  = ( c0_var_var41  | 0) + ( c0_var_var40  | 0)| 0;
     c0_var_var43  = ( c0_var_var42  | 0) + ( c0_var_var41  | 0)| 0;
     c0_var_var44  = ( c0_var_var43  | 0) + ( c0_var_var42  | 0)| 0;
     c0_var_var45  = ( c0_var_var44  | 0) + ( c0_var_var43  | 0)| 0;
     c0_var_var46  = ( c0_var_var45  | 0) + ( c0_var_var44  | 0)| 0;
     c0_var_var47  = ( c0_var_var46  | 0) + ( c0_var_var45  | 0)| 0;
     c0_var_var48  = ( c0_var_var47  | 0) + ( c0_var_var46  | 0)| 0;
     c0_var_var49  = ( c0_var_var48  | 0) + ( c0_var_var47  | 0)| 0;
     c0_var_var50  = ( c0_var_var49  | 0) + ( c0_var_var48  | 0)| 0;
     c0_var_var51  = ( c0_var_var50  | 0) + ( c0_var_var49  | 0)| 0;
     c0_var_var52  = ( c0_var_var51  | 0) + ( c0_var_var50  | 0)| 0;
     c0_var_var53  = ( c0_var_var52  | 0) + ( c0_var_var51  | 0)| 0;
     c0_var_var54  = ( c0_var_var53  | 0) + ( c0_var_var52  | 0)| 0;
     c0_var_var55  = ( c0_var_var54  | 0) + ( c0_var_var53  | 0)| 0;
     c0_var_var56  = ( c0_var_var55  | 0) + ( c0_var_var54  | 0)| 0;
     c0_var_var57  = ( c0_var_var56  | 0) + ( c0_var_var55  | 0)| 0;
     c0_var_var58  = ( c0_var_var57  | 0) + ( c0_var_var56  | 0)| 0;
     c0_var_var59  = ( c0_var_var58  | 0) + ( c0_var_var57  | 0)| 0;
     c0_var_var60  = ( c0_var_var59  | 0) + ( c0_var_var58  | 0)| 0;
     c0_var_var61  = ( c0_var_var60  | 0) + ( c0_var_var59  | 0)| 0;
     c0_var_var62  = ( c0_var_var61  | 0) + ( c0_var_var60  | 0)| 0;
     c0_var_var63  = ( c0_var_var62  | 0) + ( c0_var_var61  | 0)| 0;
     c0_var_var64  = ( c0_var_var63  | 0) + ( c0_var_var62  | 0)| 0;
     c0_var_var65  = ( c0_var_var64  | 0) + ( c0_var_var63  | 0)| 0;
     c0_var_var66  = ( c0_var_var65  | 0) + ( c0_var_var64  | 0)| 0;
     c0_var_var67  = ( c0_var_var66  | 0) + ( c0_var_var65  | 0)| 0;
     c0_var_var68  = ( c0_var_var67  | 0) + ( c0_var_var66  | 0)| 0;
     c0_var_var69  = ( c0_var_var68  | 0) + ( c0_var_var67  | 0)| 0;
     c0_var_var70  = ( c0_var_var69  | 0) + ( c0_var_var68  | 0)| 0;
     c0_var_var71  = ( c0_var_var70  | 0) + ( c0_var_var69  | 0)| 0;
     c0_var_var72  = ( c0_var_var71  | 0) + ( c0_var_var70  | 0)| 0;
     c0_var_var73  = ( c0_var_var72  | 0) + ( c0_var_var71  | 0)| 0;
     c0_var_var74  = ( c0_var_var73  | 0) + ( c0_var_var72  | 0)| 0;
     c0_var_var75  = ( c0_var_var74  | 0) + ( c0_var_var73  | 0)| 0;
     c0_var_var76  = ( c0_var_var75  | 0) + ( c0_var_var74  | 0)| 0;
     c0_var_var77  = ( c0_var_var76  | 0) + ( c0_var_var75  | 0)| 0;
     c0_var_var78  = ( c0_var_var77  | 0) + ( c0_var_var76  | 0)| 0;
     c0_var_var79  = ( c0_var_var78  | 0) + ( c0_var_var77  | 0)| 0;
     c0_var_var80  = ( c0_var_var79  | 0) + ( c0_var_var78  | 0)| 0;
     c0_var_var81  = ( c0_var_var80  | 0) + ( c0_var_var79  | 0)| 0;
     c0_var_var82  = ( c0_var_var81  | 0) + ( c0_var_var80  | 0)| 0;
     c0_var_var83  = ( c0_var_var82  | 0) + ( c0_var_var81  | 0)| 0;
     c0_var_var84  = ( c0_var_var83  | 0) + ( c0_var_var82  | 0)| 0;
     c0_var_var85  = ( c0_var_var84  | 0) + ( c0_var_var83  | 0)| 0;
     c0_var_var86  = ( c0_var_var85  | 0) + ( c0_var_var84  | 0)| 0;
     c0_var_var87  = ( c0_var_var86  | 0) + ( c0_var_var85  | 0)| 0;
     c0_var_var88  = ( c0_var_var87  | 0) + ( c0_var_var86  | 0)| 0;
     c0_var_var89  = ( c0_var_var88  | 0) + ( c0_var_var87  | 0)| 0;
     c0_var_var90  = ( c0_var_var89  | 0) + ( c0_var_var88  | 0)| 0;
     c0_var_var91  = ( c0_var_var90  | 0) + ( c0_var_var89  | 0)| 0;
     c0_var_var92  = ( c0_var_var91  | 0) + ( c0_var_var90  | 0)| 0;
     c0_var_var93  = ( c0_var_var92  | 0) + ( c0_var_var91  | 0)| 0;
     c0_var_var94  = ( c0_var_var93  | 0) + ( c0_var_var92  | 0)| 0;
     c0_var_var95  = ( c0_var_var94  | 0) + ( c0_var_var93  | 0)| 0;
     c0_var_var96  = ( c0_var_var95  | 0) + ( c0_var_var94  | 0)| 0;
     c0_var_var97  = ( c0_var_var96  | 0) + ( c0_var_var95  | 0)| 0;
     c0_var_var98  = ( c0_var_var97  | 0) + ( c0_var_var96  | 0)| 0;
     c0_var_var99  = ( c0_var_var98  | 0) + ( c0_var_var97  | 0)| 0;
     c0_var_var100  = ( c0_var_var99  | 0) + ( c0_var_var98  | 0)| 0;
     c0_var_var101  = ( c0_var_var100  | 0) + ( c0_var_var99  | 0)| 0;
     c0_var_var102  = ( c0_var_var101  | 0) + ( c0_var_var100  | 0)| 0;
     c0_var_var103  = ( c0_var_var102  | 0) + ( c0_var_var101  | 0)| 0;
     c0_var_var104  = ( c0_var_var103  | 0) + ( c0_var_var102  | 0)| 0;
     c0_var_var105  = ( c0_var_var104  | 0) + ( c0_var_var103  | 0)| 0;
     c0_var_var106  = ( c0_var_var105  | 0) + ( c0_var_var104  | 0)| 0;
     c0_var_var107  = ( c0_var_var106  | 0) + ( c0_var_var105  | 0)| 0;
     c0_var_var108  = ( c0_var_var107  | 0) + ( c0_var_var106  | 0)| 0;
     c0_var_var109  = ( c0_var_var108  | 0) + ( c0_var_var107  | 0)| 0;
     c0_var_var110  = ( c0_var_var109  | 0) + ( c0_var_var108  | 0)| 0;
     c0_var_var111  = ( c0_var_var110  | 0) + ( c0_var_var109  | 0)| 0;
     c0_var_var112  = ( c0_var_var111  | 0) + ( c0_var_var110  | 0)| 0;
     c0_var_var113  = ( c0_var_var112  | 0) + ( c0_var_var111  | 0)| 0;
     c0_var_var114  = ( c0_var_var113  | 0) + ( c0_var_var112  | 0)| 0;
     c0_var_var115  = ( c0_var_var114  | 0) + ( c0_var_var113  | 0)| 0;
     c0_var_var116  = ( c0_var_var115  | 0) + ( c0_var_var114  | 0)| 0;
     c0_var_var117  = ( c0_var_var116  | 0) + ( c0_var_var115  | 0)| 0;
     c0_var_var118  = ( c0_var_var117  | 0) + ( c0_var_var116  | 0)| 0;
     c0_var_var119  = ( c0_var_var118  | 0) + ( c0_var_var117  | 0)| 0;
     c0_var_var120  = ( c0_var_var119  | 0) + ( c0_var_var118  | 0)| 0;
     c0_var_var121  = ( c0_var_var120  | 0) + ( c0_var_var119  | 0)| 0;
     c0_var_var122  = ( c0_var_var121  | 0) + ( c0_var_var120  | 0)| 0;
     c0_var_var123  = ( c0_var_var122  | 0) + ( c0_var_var121  | 0)| 0;
     c0_var_var124  = ( c0_var_var123  | 0) + ( c0_var_var122  | 0)| 0;
     c0_var_var125  = ( c0_var_var124  | 0) + ( c0_var_var123  | 0)| 0;
     c0_var_var126  = ( c0_var_var125  | 0) + ( c0_var_var124  | 0)| 0;
     c0_var_var127  = ( c0_var_var126  | 0) + ( c0_var_var125  | 0)| 0;
     c0_var_var128  = ( c0_var_var127  | 0) + ( c0_var_var126  | 0)| 0;
     c0_var_var129  = ( c0_var_var128  | 0) + ( c0_var_var127  | 0)| 0;
     c0_var_var130  = ( c0_var_var129  | 0) + ( c0_var_var128  | 0)| 0;
     c0_var_var131  = ( c0_var_var130  | 0) + ( c0_var_var129  | 0)| 0;
     c0_var_var132  = ( c0_var_var131  | 0) + ( c0_var_var130  | 0)| 0;
     c0_var_var133  = ( c0_var_var132  | 0) + ( c0_var_var131  | 0)| 0;
     c0_var_var134  = ( c0_var_var133  | 0) + ( c0_var_var132  | 0)| 0;
     c0_var_var135  = ( c0_var_var134  | 0) + ( c0_var_var133  | 0)| 0;
     c0_var_var136  = ( c0_var_var135  | 0) + ( c0_var_var134  | 0)| 0;
     c0_var_var137  = ( c0_var_var136  | 0) + ( c0_var_var135  | 0)| 0;
     c0_var_var138  = ( c0_var_var137  | 0) + ( c0_var_var136  | 0)| 0;
     c0_var_var139  = ( c0_var_var138  | 0) + ( c0_var_var137  | 0)| 0;
     c0_var_var140  = ( c0_var_var139  | 0) + ( c0_var_var138  | 0)| 0;
     c0_var_var141  = ( c0_var_var140  | 0) + ( c0_var_var139  | 0)| 0;
     c0_var_var142  = ( c0_var_var141  | 0) + ( c0_var_var140  | 0)| 0;
     c0_var_var143  = ( c0_var_var142  | 0) + ( c0_var_var141  | 0)| 0;
     c0_var_var144  = ( c0_var_var143  | 0) + ( c0_var_var142  | 0)| 0;
     c0_var_var145  = ( c0_var_var144  | 0) + ( c0_var_var143  | 0)| 0;
     c0_var_var146  = ( c0_var_var145  | 0) + ( c0_var_var144  | 0)| 0;
     c0_var_var147  = ( c0_var_var146  | 0) + ( c0_var_var145  | 0)| 0;
     c0_var_var148  = ( c0_var_var147  | 0) + ( c0_var_var146  | 0)| 0;
     c0_var_var149  = ( c0_var_var148  | 0) + ( c0_var_var147  | 0)| 0;
     c0_var_var150  = ( c0_var_var149  | 0) + ( c0_var_var148  | 0)| 0;
     c0_var_var151  = ( c0_var_var150  | 0) + ( c0_var_var149  | 0)| 0;
     c0_var_var152  = ( c0_var_var151  | 0) + ( c0_var_var150  | 0)| 0;
     c0_var_var153  = ( c0_var_var152  | 0) + ( c0_var_var151  | 0)| 0;
     c0_var_var154  = ( c0_var_var153  | 0) + ( c0_var_var152  | 0)| 0;
     c0_var_var155  = ( c0_var_var154  | 0) + ( c0_var_var153  | 0)| 0;
     c0_var_var156  = ( c0_var_var155  | 0) + ( c0_var_var154  | 0)| 0;
     c0_var_var157  = ( c0_var_var156  | 0) + ( c0_var_var155  | 0)| 0;
     c0_var_var158  = ( c0_var_var157  | 0) + ( c0_var_var156  | 0)| 0;
     c0_var_var159  = ( c0_var_var158  | 0) + ( c0_var_var157  | 0)| 0;
     c0_var_var160  = ( c0_var_var159  | 0) + ( c0_var_var158  | 0)| 0;
     c0_var_var161  = ( c0_var_var160  | 0) + ( c0_var_var159  | 0)| 0;
     c0_var_var162  = ( c0_var_var161  | 0) + ( c0_var_var160  | 0)| 0;
     c0_var_var163  = ( c0_var_var162  | 0) + ( c0_var_var161  | 0)| 0;
     c0_var_var164  = ( c0_var_var163  | 0) + ( c0_var_var162  | 0)| 0;
     c0_var_var165  = ( c0_var_var164  | 0) + ( c0_var_var163  | 0)| 0;
     c0_var_var166  = ( c0_var_var165  | 0) + ( c0_var_var164  | 0)| 0;
     c0_var_var167  = ( c0_var_var166  | 0) + ( c0_var_var165  | 0)| 0;
     c0_var_var168  = ( c0_var_var167  | 0) + ( c0_var_var166  | 0)| 0;
     c0_var_var169  = ( c0_var_var168  | 0) + ( c0_var_var167  | 0)| 0;
     c0_var_var170  = ( c0_var_var169  | 0) + ( c0_var_var168  | 0)| 0;
     c0_var_var171  = ( c0_var_var170  | 0) + ( c0_var_var169  | 0)| 0;
     c0_var_var172  = ( c0_var_var171  | 0) + ( c0_var_var170  | 0)| 0;
     c0_var_var173  = ( c0_var_var172  | 0) + ( c0_var_var171  | 0)| 0;
     c0_var_var174  = ( c0_var_var173  | 0) + ( c0_var_var172  | 0)| 0;
     c0_var_var175  = ( c0_var_var174  | 0) + ( c0_var_var173  | 0)| 0;
     c0_var_var176  = ( c0_var_var175  | 0) + ( c0_var_var174  | 0)| 0;
     c0_var_var177  = ( c0_var_var176  | 0) + ( c0_var_var175  | 0)| 0;
     c0_var_var178  = ( c0_var_var177  | 0) + ( c0_var_var176  | 0)| 0;
     c0_var_var179  = ( c0_var_var178  | 0) + ( c0_var_var177  | 0)| 0;
     c0_var_var180  = ( c0_var_var179  | 0) + ( c0_var_var178  | 0)| 0;
     c0_var_var181  = ( c0_var_var180  | 0) + ( c0_var_var179  | 0)| 0;
     c0_var_var182  = ( c0_var_var181  | 0) + ( c0_var_var180  | 0)| 0;
     c0_var_var183  = ( c0_var_var182  | 0) + ( c0_var_var181  | 0)| 0;
     c0_var_var184  = ( c0_var_var183  | 0) + ( c0_var_var182  | 0)| 0;
     c0_var_var185  = ( c0_var_var184  | 0) + ( c0_var_var183  | 0)| 0;
     c0_var_var186  = ( c0_var_var185  | 0) + ( c0_var_var184  | 0)| 0;
     c0_var_var187  = ( c0_var_var186  | 0) + ( c0_var_var185  | 0)| 0;
     c0_var_var188  = ( c0_var_var187  | 0) + ( c0_var_var186  | 0)| 0;
     c0_var_var189  = ( c0_var_var188  | 0) + ( c0_var_var187  | 0)| 0;
     c0_var_var190  = ( c0_var_var189  | 0) + ( c0_var_var188  | 0)| 0;
     c0_var_var191  = ( c0_var_var190  | 0) + ( c0_var_var189  | 0)| 0;
     c0_var_var192  = ( c0_var_var191  | 0) + ( c0_var_var190  | 0)| 0;
     c0_var_var193  = ( c0_var_var192  | 0) + ( c0_var_var191  | 0)| 0;
     c0_var_var194  = ( c0_var_var193  | 0) + ( c0_var_var192  | 0)| 0;
     c0_var_var195  = ( c0_var_var194  | 0) + ( c0_var_var193  | 0)| 0;
     c0_var_var196  = ( c0_var_var195  | 0) + ( c0_var_var194  | 0)| 0;
     c0_var_var197  = ( c0_var_var196  | 0) + ( c0_var_var195  | 0)| 0;
     c0_var_var198  = ( c0_var_var197  | 0) + ( c0_var_var196  | 0)| 0;
     c0_var_var199  = ( c0_var_var198  | 0) + ( c0_var_var197  | 0)| 0;
     c0_var_var200  = ( c0_var_var199  | 0) + ( c0_var_var198  | 0)| 0;
     c0_var_var201  = ( c0_var_var200  | 0) + ( c0_var_var199  | 0)| 0;
     c0_var_var202  = ( c0_var_var201  | 0) + ( c0_var_var200  | 0)| 0;
     c0_var_var203  = ( c0_var_var202  | 0) + ( c0_var_var201  | 0)| 0;
     c0_var_var204  = ( c0_var_var203  | 0) + ( c0_var_var202  | 0)| 0;
     c0_var_var205  = ( c0_var_var204  | 0) + ( c0_var_var203  | 0)| 0;
     c0_var_var206  = ( c0_var_var205  | 0) + ( c0_var_var204  | 0)| 0;
     c0_var_var207  = ( c0_var_var206  | 0) + ( c0_var_var205  | 0)| 0;
     c0_var_var208  = ( c0_var_var207  | 0) + ( c0_var_var206  | 0)| 0;
     c0_var_var209  = ( c0_var_var208  | 0) + ( c0_var_var207  | 0)| 0;
     c0_var_var210  = ( c0_var_var209  | 0) + ( c0_var_var208  | 0)| 0;
     c0_var_var211  = ( c0_var_var210  | 0) + ( c0_var_var209  | 0)| 0;
     c0_var_var212  = ( c0_var_var211  | 0) + ( c0_var_var210  | 0)| 0;
     c0_var_var213  = ( c0_var_var212  | 0) + ( c0_var_var211  | 0)| 0;
     c0_var_var214  = ( c0_var_var213  | 0) + ( c0_var_var212  | 0)| 0;
     c0_var_var215  = ( c0_var_var214  | 0) + ( c0_var_var213  | 0)| 0;
     c0_var_var216  = ( c0_var_var215  | 0) + ( c0_var_var214  | 0)| 0;
     c0_var_var217  = ( c0_var_var216  | 0) + ( c0_var_var215  | 0)| 0;
     c0_var_var218  = ( c0_var_var217  | 0) + ( c0_var_var216  | 0)| 0;
     c0_var_var219  = ( c0_var_var218  | 0) + ( c0_var_var217  | 0)| 0;
     c0_var_var220  = ( c0_var_var219  | 0) + ( c0_var_var218  | 0)| 0;
     c0_var_var221  = ( c0_var_var220  | 0) + ( c0_var_var219  | 0)| 0;
     c0_var_var222  = ( c0_var_var221  | 0) + ( c0_var_var220  | 0)| 0;
     c0_var_var223  = ( c0_var_var222  | 0) + ( c0_var_var221  | 0)| 0;
     c0_var_var224  = ( c0_var_var223  | 0) + ( c0_var_var222  | 0)| 0;
     c0_var_var225  = ( c0_var_var224  | 0) + ( c0_var_var223  | 0)| 0;
     c0_var_var226  = ( c0_var_var225  | 0) + ( c0_var_var224  | 0)| 0;
     c0_var_var227  = ( c0_var_var226  | 0) + ( c0_var_var225  | 0)| 0;
     c0_var_var228  = ( c0_var_var227  | 0) + ( c0_var_var226  | 0)| 0;
     c0_var_var229  = ( c0_var_var228  | 0) + ( c0_var_var227  | 0)| 0;
     c0_var_var230  = ( c0_var_var229  | 0) + ( c0_var_var228  | 0)| 0;
     c0_var_var231  = ( c0_var_var230  | 0) + ( c0_var_var229  | 0)| 0;
     c0_var_var232  = ( c0_var_var231  | 0) + ( c0_var_var230  | 0)| 0;
     c0_var_var233  = ( c0_var_var232  | 0) + ( c0_var_var231  | 0)| 0;
     c0_var_var234  = ( c0_var_var233  | 0) + ( c0_var_var232  | 0)| 0;
     c0_var_var235  = ( c0_var_var234  | 0) + ( c0_var_var233  | 0)| 0;
     c0_var_var236  = ( c0_var_var235  | 0) + ( c0_var_var234  | 0)| 0;
     c0_var_var237  = ( c0_var_var236  | 0) + ( c0_var_var235  | 0)| 0;
     c0_var_var238  = ( c0_var_var237  | 0) + ( c0_var_var236  | 0)| 0;
     c0_var_var239  = ( c0_var_var238  | 0) + ( c0_var_var237  | 0)| 0;
     c0_var_var240  = ( c0_var_var239  | 0) + ( c0_var_var238  | 0)| 0;
     c0_var_var241  = ( c0_var_var240  | 0) + ( c0_var_var239  | 0)| 0;
     c0_var_var242  = ( c0_var_var241  | 0) + ( c0_var_var240  | 0)| 0;
     c0_var_var243  = ( c0_var_var242  | 0) + ( c0_var_var241  | 0)| 0;
     c0_var_var244  = ( c0_var_var243  | 0) + ( c0_var_var242  | 0)| 0;
     c0_var_var245  = ( c0_var_var244  | 0) + ( c0_var_var243  | 0)| 0;
     c0_var_var246  = ( c0_var_var245  | 0) + ( c0_var_var244  | 0)| 0;
     c0_var_var247  = ( c0_var_var246  | 0) + ( c0_var_var245  | 0)| 0;
     c0_var_var248  = ( c0_var_var247  | 0) + ( c0_var_var246  | 0)| 0;
     c0_var_var249  = ( c0_var_var248  | 0) + ( c0_var_var247  | 0)| 0;
     c0_var_var250  = ( c0_var_var249  | 0) + ( c0_var_var248  | 0)| 0;
     c0_var_var251  = ( c0_var_var250  | 0) + ( c0_var_var249  | 0)| 0;
     c0_var_var252  = ( c0_var_var251  | 0) + ( c0_var_var250  | 0)| 0;
     c0_var_var253  = ( c0_var_var252  | 0) + ( c0_var_var251  | 0)| 0;
     c0_var_var254  = ( c0_var_var253  | 0) + ( c0_var_var252  | 0)| 0;
     c0_var_var255  = ( c0_var_var254  | 0) + ( c0_var_var253  | 0)| 0;
    return ( c0_var_var255  | 0);

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