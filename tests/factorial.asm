mov cx, 6      
call fact      
msg '------------'
msg 'the factorial of ', cx, ' is: ', ax
msg '------------'
end

fact:
  push cx
  mov ax, 1   

  cmp cx, 0
  je end_fact

fact_loop:
  mul cx    
  dec cx
  cmp cx, 1
  jge fact_loop

end_fact:
  pop cx
  ret
