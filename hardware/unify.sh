if [ ! -d "unify" ]; then
  mkdir unify
fi

cat \
 embedded_la/config_pkg.vhd \
 logic_analyzer/logic_analyzer_pkg.vhd \
 embedded_la/embedded_la_pkg.vhd \
 logic_analyzer/wishbone.vhd \
 logic_analyzer/interal_pkg.vhd \
 logic_analyzer/txt_util.vhd \
 rs232_controller/rs232_controller_pkg.vhd \
 rs232_controller/ucrc_pkg.vhd \
 \
 logic_analyzer/clock_divider.vhd \
 logic_analyzer/input_multiplexer.vhd \
 logic_analyzer/run_length_encoder.vhd \
 logic_analyzer/trigger_edge.vhd \
 logic_analyzer/input_shift_register.vhd \
 logic_analyzer/trigger_value.vhd \
 logic_analyzer/dual_input_sr.vhd \
 logic_analyzer/memory_manager.vhd \
 logic_analyzer/test_pattern_gen.vhd \
 logic_analyzer/trigger.vhd \
 logic_analyzer/logic_analyzer_core.vhd \
 \
 rs232_controller/char_to_hex.vhd \
 rs232_controller/Rxunit.vhd \
 rs232_controller/Txunit.vhd \
 rs232_controller/utils.vhd \
 rs232_controller/hex_to_char.vhd \
 rs232_controller/shift_register.vhd \
 rs232_controller/ucrc_par.vhd \
 rs232_controller/miniuart.vhd \
 rs232_controller/rs232_fsm.vhd \
 rs232_controller/rs232_controller.vhd \
 \
 embedded_la/sampler_memory.vhd \
 embedded_la/la_embedding.vhd \
> unify/tmp

sed \
 -e 's/LOGIC_ANALYZER\./EMBEDDED_LA\./g' \
 -e 's/ LOGIC_ANALYZER;/ EMBEDDED_LA;/g' \
 -e 's/RS232_CONTROLLER\./EMBEDDED_LA\./g' \
 -e 's/ RS232_CONTROLLER;/ EMBEDDED_LA;/g' \
 -e 's/SOC_SPARTAN3\./EMBEDDED_LA\./g' \
 -e 's/ SOC_SPARTAN3;/ EMBEDDED_LA;/g' \
 -e 's/^--.*$//g' \
 -e 's/;--.*$/;/g' \
unify/tmp > unify/tmp1

echo "---------------------------------------------------
-- Embedded Logic Analyzer
-- Includes codes from:
--  -> http://opencores.org/project,miniuart2 downloaded 29.07.2011
--  -> http://www.stefanvhdl.com/vhdl/vhdl/txt_util.vhd downloaded 15.09.2011
---------------------------------------------------
" > unify/header

cat -s unify/header unify/tmp1 > unify/embedded_la.vhd

rm unify/tmp*
rm unify/header

