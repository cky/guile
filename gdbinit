# -*- GDB-Script -*-

handle SIGPWR noprint nostop
handle SIGXCPU noprint nostop

define newline
  call (void)scm_newline (scm_current_error_port ())
end

define pp
  call (void)scm_call_1 (scm_variable_ref (scm_c_module_lookup (scm_c_resolve_module ("ice-9 pretty-print"), "pretty-print")), $arg0)
end

define gdisplay
  call (void)scm_display ($arg0, scm_current_error_port ())
  newline
end

define gwrite
  call (void)scm_write ($arg0, scm_current_error_port ())
  newline
end

define sputs
  call (void)scm_puts ($arg0, scm_current_error_port ())
end

define gslot
  print ((SCM**)$arg0)[1][$arg1]
end

define pslot
  gslot $arg0 $arg1
  gwrite $
end

define lforeach
  set $l=$arg0
  while $l != 0x404
    set $x=scm_car($l)
    $arg1 $x
    set $l = scm_cdr($l)
  end
end

define modsum
  modname $arg0
  gslot $arg0 1
  set $uses=$
  output "uses:\n"
  lforeach $uses modname
end

define moduses
  pslot $arg0 1
end

define modname
  pslot $arg0 5
end

define modkind
  pslot $arg0 6
end

define car
  call scm_car ($arg0)
end

define cdr
  call scm_cdr ($arg0)
end

define smobwordtox
  set $x=((SCM*)$arg0)[$arg1]
end

define smobdatatox
  smobwordtox $arg0 1
end

define program_objcode
  smobdatatox $arg0
  set $objcode=$x
  smobdatatox $objcode
  p *(struct scm_objcode*)$x
end

define proglocals
  set $i=bp->nlocs
  while $i > 0
    set $i=$i-1
    gwrite fp[bp->nargs+$i]
  end
end

define progstack
  set $x=sp
  while $x > stack_base
    gwrite *$x
    set $x=$x-1
  end
end

define tc16
  p ((scm_t_bits)$arg0) & 0xffff
end

define smobdescriptor
  p scm_smobs[0xff & (((scm_t_bits)$arg0) >> 8)]
end

define vmstackinit
  set $vmsp=sp
  set $vmstack_base=stack_base
  set $vmfp=fp
  set $vmbp=bp
  set $vmframe=0
end

define nextframe
  set $orig_vmsp=$vmsp
  while $vmsp > $vmstack_base
    output $orig_vmsp - $vmsp
    sputs "\t"
    output $vmsp
    sputs "\t"
    gwrite *$vmsp
    set $vmsp=$vmsp-1
  end
  newline
  sputs "Frame "
  output $vmframe
  newline
  sputs "ra:\t"
  output $vmsp
  sputs "\t"
  output (SCM*)*$vmsp
  set $vmsp=$vmsp-1
  newline
  sputs "mvra:\t"
  output $vmsp
  sputs "\t"
  output (SCM*)*$vmsp
  set $vmsp=$vmsp-1
  newline
  sputs "dl:\t"
  output $vmsp
  sputs "\t"
  set $vmdl=(SCM*)(*$vmsp)
  output $vmdl
  newline
  set $vmsp=$vmsp-1
  set $vmnlocs=(int)$vmbp->nlocs
  while $vmnlocs > 0
    sputs "loc #"
    output $vmnlocs
    sputs ":\t"
    output $vmsp
    sputs "\t"
    gwrite *$vmsp
    set $vmsp=$vmsp-1
    set $vmnlocs=$vmnlocs-1
  end
  set $vmnargs=(int)$vmbp->nargs
  while $vmnargs > 0
    sputs "arg #"
    output $vmnargs
    sputs ":\t"
    output $vmsp
    sputs "\t"
    gwrite *$vmsp
    set $vmsp=$vmsp-1
    set $vmnargs=$vmnargs-1
  end
  sputs "prog:\t"
  output $vmsp
  sputs "\t"
  gwrite *$vmsp
  set $vmsp=$vmsp-1
  newline
  if $vmdl
    set $vmfp=$vmdl
    set $vmbp=(struct scm_objcode*)((SCM*)(((SCM*)($vmfp[-1]))[1])[1])
    set $vmstack_base=$vmfp+$vmbp->nargs+$vmbp->nlocs+4
    set $vmframe=$vmframe+1
    newline
  end
end

define vmstack
  vmstackinit
  while $vmsp > vp->stack_base
    nextframe
  end
end

define inst
  p scm_instruction_table[$arg0]
end

define gbt
  call scm_display_backtrace (scm_make_stack(0x404,0x304), scm_current_error_port (), 0x704, 0x704, 0x704)
end
