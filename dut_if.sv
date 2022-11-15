`timescale 1 ns/1 ps

`include "axi/typedef.svh"
`include "axi/assign.svh"

module dut_if 
#(
  parameter AXI_ADDR_WIDTH = 64,
  parameter AXI_DATA_WIDTH = 64,
  parameter AXI_USER_WIDTH = 6,
  parameter AXI_ID_WIDTH   = 6,
  parameter NUM_ROWS       = 1024
)
(
  input logic          clk_i,
  input logic          rst_ni,
  input logic          test_en_i,
  AXI_BUS_DV.Slave     axi_slave
);

  localparam NB_DMAS =4;

  logic [NB_DMAS-1:0][31:0] s_tcdm_bus_wdata;
  logic [NB_DMAS-1:0][31:0] s_tcdm_bus_add;
  logic [NB_DMAS-1:0]       s_tcdm_bus_req;
  logic [NB_DMAS-1:0]       s_tcdm_bus_wen;
  logic [NB_DMAS-1:0][3:0]  s_tcdm_bus_be;
  logic [NB_DMAS-1:0]       s_tcdm_bus_gnt;
  logic [NB_DMAS-1:0][31:0] s_tcdm_bus_r_rdata;
  logic [NB_DMAS-1:0]       s_tcdm_bus_r_valid;

  axi2tcdm #(
    .AXI_ADDR_WIDTH       ( AXI_ADDR_WIDTH        ),
    .AXI_DATA_WIDTH       ( AXI_DATA_WIDTH        ),
    .AXI_USER_WIDTH       ( AXI_USER_WIDTH        ),
    .AXI_ID_WIDTH         ( AXI_ID_WIDTH          )
  ) axi2mem_i (
    .clk_i                 ( clk_i                ),
    .rst_ni                ( rst_ni               ),

    .tcdm_master_req_o     ( s_tcdm_bus_req       ),
    .tcdm_master_add_o     ( s_tcdm_bus_add       ),
    .tcdm_master_type_o    ( s_tcdm_bus_wen       ),
    .tcdm_master_data_o    ( s_tcdm_bus_wdata     ),
    .tcdm_master_be_o      ( s_tcdm_bus_be        ),
    .tcdm_master_gnt_i     ( s_tcdm_bus_gnt       ),

    .tcdm_master_r_valid_i ( s_tcdm_bus_r_valid   ),
    .tcdm_master_r_data_i  ( s_tcdm_bus_r_rdata   ),

    .busy_o                ( busy_o               ),
    .test_en_i             ( test_en_i            ),

    .axi_slave_aw_valid_i  ( axi_slave.aw_valid   ),
    .axi_slave_aw_addr_i   ( axi_slave.aw_addr    ),
    .axi_slave_aw_prot_i   ( axi_slave.aw_prot    ),
    .axi_slave_aw_region_i ( axi_slave.aw_region  ),
    .axi_slave_aw_len_i    ( axi_slave.aw_len     ),
    .axi_slave_aw_size_i   ( axi_slave.aw_size    ),
    .axi_slave_aw_burst_i  ( axi_slave.aw_burst   ),
    .axi_slave_aw_lock_i   ( axi_slave.aw_lock    ),
    .axi_slave_aw_cache_i  ( axi_slave.aw_cache   ),
    .axi_slave_aw_qos_i    ( axi_slave.aw_qos     ),
    .axi_slave_aw_id_i     ( axi_slave.aw_id      ),
    .axi_slave_aw_user_i   ( axi_slave.aw_user    ),
    .axi_slave_aw_ready_o  ( axi_slave.aw_ready   ),

    .axi_slave_ar_valid_i  ( axi_slave.ar_valid   ),
    .axi_slave_ar_addr_i   ( axi_slave.ar_addr    ),
    .axi_slave_ar_prot_i   ( axi_slave.ar_prot    ),
    .axi_slave_ar_region_i ( axi_slave.ar_region  ),
    .axi_slave_ar_len_i    ( axi_slave.ar_len     ),
    .axi_slave_ar_size_i   ( axi_slave.ar_size    ),
    .axi_slave_ar_burst_i  ( axi_slave.ar_burst   ),
    .axi_slave_ar_lock_i   ( axi_slave.ar_lock    ),
    .axi_slave_ar_cache_i  ( axi_slave.ar_cache   ),
    .axi_slave_ar_qos_i    ( axi_slave.ar_qos     ),
    .axi_slave_ar_id_i     ( axi_slave.ar_id      ),
    .axi_slave_ar_user_i   ( axi_slave.ar_user    ),
    .axi_slave_ar_ready_o  ( axi_slave.ar_ready   ),

    .axi_slave_w_valid_i   ( axi_slave.w_valid    ),
    .axi_slave_w_data_i    ( axi_slave.w_data     ),
    .axi_slave_w_strb_i    ( axi_slave.w_strb     ),
    .axi_slave_w_user_i    ( axi_slave.w_user     ),
    .axi_slave_w_last_i    ( axi_slave.w_last     ),
    .axi_slave_w_ready_o   ( axi_slave.w_ready    ),

    .axi_slave_r_valid_o   ( axi_slave.r_valid    ),
    .axi_slave_r_data_o    ( axi_slave.r_data     ),
    .axi_slave_r_resp_o    ( axi_slave.r_resp     ),
    .axi_slave_r_last_o    ( axi_slave.r_last     ),
    .axi_slave_r_id_o      ( axi_slave.r_id       ),
    .axi_slave_r_user_o    ( axi_slave.r_user     ),
    .axi_slave_r_ready_i   ( axi_slave.r_ready    ),

    .axi_slave_b_valid_o   ( axi_slave.b_valid    ),
    .axi_slave_b_resp_o    ( axi_slave.b_resp     ),
    .axi_slave_b_id_o      ( axi_slave.b_id       ),
    .axi_slave_b_user_o    ( axi_slave.b_user     ),
    .axi_slave_b_ready_i   ( axi_slave.b_ready    )
  );
            
  // BEHAV MEMORY
  logic [31:0] mem_q [NUM_ROWS-1:0];
   
  always_comb begin
     for(int unsigned j = 0; j<NB_DMAS; j++)
       s_tcdm_bus_gnt[j] = s_tcdm_bus_req[j];
  end

  always_ff @(posedge clk_i) begin
     for (int unsigned i = 0; i<NB_DMAS; i++) begin
        s_tcdm_bus_r_rdata[i] = '0;
        s_tcdm_bus_r_valid[i] = 1'b0;        
        if(s_tcdm_bus_req[i]) begin
           if(~s_tcdm_bus_wen[i]) begin
              for (int unsigned k = 0; k<4; k++) begin
                 if(s_tcdm_bus_be[i][k]) begin
                    mem_q[(s_tcdm_bus_add[i]>>2)][k*8+:8]=s_tcdm_bus_wdata[i][k*8+:8];
                 end
              end
           end else begin
              s_tcdm_bus_r_rdata[i] = mem_q[(s_tcdm_bus_add[i]>>2)];
              s_tcdm_bus_r_valid[i] = 1'b1;
           end
        end
     end
  end 

endmodule
