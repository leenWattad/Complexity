%.o: %.asm
	echo "Assembling $< to $@"
	nasm -f elf64 -o $@ $< -g

%: %.o
	echo "Linking $< to $@"
	gcc -no-pie -o $@ $< -g -fno-pie

.PRECIOUS: %.o