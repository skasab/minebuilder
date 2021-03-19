import sys
try:
	from mcpi.minecraft import Minecraft
	mc = Minecraft.create()
except:
	print("ERROR")
	sys.exit(1)