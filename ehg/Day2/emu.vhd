LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

ENTITY emu IS
PORT(
  clk           : IN  std_ulogic;
  reset         : IN  std_ulogic;
  ram_address   : OUT std_ulogic_vector(7 DOWNTO 0);
  ram_strobe    : OUT std_ulogic;
  ram_wren      : OUT std_ulogic;
  ram_writedata : OUT std_ulogic_vector(31 DOWNTO 0);
  ram_readdata  : IN  std_ulogic_vector(31 DOWNTO 0);
  return_value  : OUT std_ulogic_vector(31 DOWNTO 0);
  return_valid  : OUT std_ulogic
);
END ENTITY;

ARCHITECTURE fsm OF emu IS

  TYPE state_type IS (idle, load_instruction, load_ref1, load_ref2, load_ref3, load_data1, load_data2, catch_data1, catch_data2, decode, store, wait_result, return_result);

  SIGNAL state : state_type;
  SIGNAL pc    : UNSIGNED(7 DOWNTO 0);

  SIGNAL operator         : UNSIGNED(31 DOWNTO 0);
  SIGNAL ref1, ref2, ref3 : std_ulogic_vector(31 DOWNTO 0);
  SIGNAL data1, data2     : UNSIGNED(31 DOWNTO 0);
  SIGNAL result           : UNSIGNED(63 DOWNTO 0);

BEGIN

  PROCESS(clk, reset)
  BEGIN
    IF rising_edge(clk) THEN
      -- Defaults
      ram_strobe <= '0';

      CASE state IS
        WHEN idle =>
          state <= load_instruction;
          pc    <= (OTHERS => '0');
        WHEN load_instruction =>
          ram_address <= std_ulogic_vector(pc);
          ram_strobe  <= '1';
          ram_wren    <= '0';
          state <= load_ref1;
        WHEN load_ref1 =>
          ram_address <= std_ulogic_vector(pc+1);
          ram_strobe  <= '1';
          ram_wren    <= '0';
          state <= load_ref2;
        WHEN load_ref2 =>
          ram_address <= std_ulogic_vector(pc+2);
          ram_strobe  <= '1';
          ram_wren    <= '0';
          state <= load_ref3;
          -- Catch operator
          operator <= UNSIGNED(ram_readdata);
        WHEN load_ref3 =>
          ram_address <= std_ulogic_vector(pc+3);
          ram_strobe  <= '1';
          ram_wren    <= '0';
          state <= load_data1;
          -- Catch first reference
          ref1 <= ram_readdata;

          -- Operator 99 means skip other stuff and return the 0th memory entry
          IF operator = 99 THEN
            ram_address <= (OTHERS => '0');
            state <= wait_result;
          END IF;
        WHEN load_data1 =>
          ram_address <= ref1(7 DOWNTO 0);
          ram_strobe  <= '1';
          ram_wren    <= '0';
          state <= load_data2;
          -- Catch second reference
          ref2 <= ram_readdata;
        WHEN load_data2 =>
          ram_address <= ref1(7 DOWNTO 0);
          ram_strobe  <= '1';
          ram_wren    <= '0';
          state <= catch_data1;
          -- Catch third reference
          ref3 <= ram_readdata;
        WHEN catch_data1 =>
          state <= catch_data2;
          data1 <= UNSIGNED(ram_readdata);
        WHEN catch_data2 =>
          state <= decode;
          data2 <= UNSIGNED(ram_readdata);
        WHEN decode =>
          CASE TO_INTEGER(operator) IS
            WHEN 1  => result(31 DOWNTO 0) <= data1 + data2;
            WHEN 2  => result <= data1 * data2;
            WHEN OTHERS  => REPORT "Bad operator" SEVERITY FAILURE;
          END CASE;
          state <= store;
        WHEN store =>
          ram_address <= ref3(7 DOWNTO 0);
          ram_strobe  <= '1';
          ram_wren    <= '1';
          ram_writedata <= std_ulogic_vector(result(31 DOWNTO 0));
          state       <= load_instruction;
          pc          <= pc + 4;
        WHEN wait_result =>
          state <= return_result;
        WHEN return_result =>
          return_value <= ram_readdata;
          return_valid <= '1';
          state <= idle;
      END CASE;
    END IF;
    IF reset = '1' THEN
      state <= idle;
    END IF;
  END PROCESS;

END ARCHITECTURE;
