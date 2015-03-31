DEVNODES?=4

all: deps
	rebar compile

deps:
	rebar get-deps

test:
	rebar eunit

distclean: clean devclean relclean
	rebar delete-deps

relclean:
	rm -rf rel/ramp

clean:
	rebar clean

stage: rel
	$(foreach dep,$(wildcard deps/*), rm -rf rel/ramp/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/ramp/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf rel/ramp/lib/$(shell basename $(app))-* && ln -sf $(abspath $(app)) rel/ramp/lib;)

rel: all
	rebar generate

# 'seq' is not available on all *BSD, so using an alternate in awk
SEQ = $(shell awk 'BEGIN { for (i = 1; i < '$(DEVNODES)'; i++) printf("%i ", i); print i ;exit(0);}')

$(eval stagedevrel: $(foreach n, $(SEQ), stagedev$(n)))
$(eval devrel: $(foreach n, $(SEQ), dev$(n)))

dev%: all
	mkdir -p dev
	rel/gen_dev $@ rel/vars/dev_vars.config.src rel/vars/$@_vars.config
	(cd rel && rebar generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

stagedev%: dev%
	$(foreach dep,$(wildcard deps/*), rm -rf dev/$^/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) dev/$^/lib;)
	$(foreach app,$(wildcard apps/*), rm -rf dev/$^/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) dev/$^/lib;)

devclean: clean
	rm -rf dev

.PHONY: test clean deps relclean devclean stagedevrel devrel
