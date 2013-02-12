SUBDIRS = stm scala java scoop

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS): 
	$(MAKE) -C $@

