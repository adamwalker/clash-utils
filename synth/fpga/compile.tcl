#Outputs go in outputs directory
set ::output_dir "./outputs"
file mkdir $::output_dir

#Create the project
create_project -part xc7a35ticsg324-1l -in_memory 

#Read the sources
read_verilog -quiet [glob -nocomplain -directory verilog/Main.top *.v]
read_xdc ../../fpga/timing.xdc

#Synthesize the design
synth_design -top top -flatten_hierarchy rebuilt
write_checkpoint -force "${::output_dir}/post_synth.dcp"

#Continue with implementation
opt_design
write_checkpoint -force "${::output_dir}/post_opt.dcp"

place_design -directive Explore
write_checkpoint -force "${::output_dir}/post_place.dcp"

phys_opt_design -directive AggressiveExplore
write_checkpoint -force "${::output_dir}/post_phys_opt.dcp"

route_design -directive Explore -tns_cleanup
write_checkpoint -force "${::output_dir}/post_route.dcp"

phys_opt_design -directive Explore
write_checkpoint -force "${::output_dir}/post_route_phys_opt.dcp"

#Reports
report_clocks -file "${::output_dir}/clocks.rpt"
report_timing_summary -file "${::output_dir}/timing.rpt"
report_utilization -file "${::output_dir}/utilization.rpt"

