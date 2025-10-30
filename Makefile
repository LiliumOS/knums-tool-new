
COMPRESSED_OUTPUTS := builtins.imtb.zst builtins.imtb.xz builtins.imtb.gz

all: builtins.imtb $(COMPRESSED_OUTPUTS)
	

builtins.imtb:
	cargo run -- --bundle builtins/ builtins.imtb

builtins.imtb.zst: builtins.imtb
	zstd -9 -kf builtins.imtb

builtins.imtb.xz: builtins.imtb
	xz -9 -kf builtins.imtb

builtins.imtb.gz: builtins.imtb
	gzip -9 -kf builtins.imtb