ls -l
type obabel
obable -i OEC_model.sdf -o oec.pdb
obabel -i OEC_model.sdf -o oec.pdb
obabel -i OEC_model.sdf -O oec.pdb
ls -l
obabel OEC_model.sdf -O oec.pdb
obabel oec+water/HOH_model.sdf oec+water/HOH.pdb
obabel oec+water/HOH_model.sdf -O oec+water/HOH.pdb
