default:
	@echo targets:  babel


MOUNT.DIR=/Users/paul/github/oec/explore/three/

# once in the container:  obabel OEC_model.sdf -O oec.pdb

babel:
	docker run \
          -v $(MOUNT.DIR):/home/obabel \
          --rm -it informaticsmatters/obabel bash

