
## Sweep through operations in DVI file and create grobs from operations

## Default is to do nothing
for (i in 0:255) {
    assign(paste0("grob_op_", i), op_ignore)
}

## 0..127
## set_char_<i>
for (i in 0:127) {
    assign(paste0("grob_op_", i), op_set_char)
}

## 128..131
## set1
## set2
## set3
## set4
grob_op_128 <- op_set
grob_op_129 <- op_set
grob_op_130 <- op_set
grob_op_131 <- op_set

## 132
## set_rule
grob_op_132 <- op_set_rule

## 133..136
## put1
## put2
## put3
## put4

## 137
## put_rule

## 138
## nop

## 139
## bop
grob_op_139 <- op_bop

## 140
## eop

## 141
## push
grob_op_141 <- op_push

## 142
## pop
grob_op_142 <- op_pop

## 143..146
## right1
## right2
## right3
## right4
for (i in 143:146) {
    assign(paste0("grob_op_", i), op_right)
}

## 147..151
## w0
## w1
## w2
## w3
## w4
for (i in 147:151) {
    assign(paste0("grob_op_", i), op_w)
}

## 152..156
## x0
## x1
## x2
## x3
## x4
for (i in 152:156) {
    assign(paste0("grob_op_", i), op_x)
}

## 157..160
## down1
## down2
## down3
## down4
for (i in 157:160) {
    assign(paste0("grob_op_", i), op_down)
}

## 161..165
## y0
## y1
## y2
## y3
## y4
for (i in 161:165) {
    assign(paste0("grob_op_", i), op_y)
}

## 166..170
## z0
## z1
## z2
## z3
## z4
for (i in 166:170) {
    assign(paste0("grob_op_", i), op_z)
}

## 171..234
## fnt_num_<i-170>
for (i in 171:234) {
    assign(paste0("grob_op_", i), op_fnt_num)
}

## 235..238
## fnt1
## fnt2
## fnt3
## fnt4

## 239..242
## xxx1
## xxx2
## xxx3
## xxx4

## 243..246
## fnt_def_1
## fnt_def_2
## fnt_def_3
## fnt_def_4
grob_op_243 <- op_font_def
grob_op_244 <- op_font_def
grob_op_245 <- op_font_def
grob_op_246 <- op_font_def

## 247
## pre
grob_op_247 <- op_pre

## upTeX
## 255
## dir
grob_op_255 <- op_dir

grobDVI <- function(op) {
    opcode <- blockValue(op$blocks$op.opcode)
    base::get(paste0("grob_op_", opcode))(op)
}

