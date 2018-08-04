# SoftCore_Present80_Accel
Present-80 Lightweight Block Cipher for Soft Core with Accelerators

Based on Soft Core Microprocessor William Diehl Version 1.3 07-23-2018

I. Contents:

    Program Loader (PROGLOAD.vhd) and Data Loader (DATALOAD.vhd) Soft Core VHDL source files
    Updated soft core datapath (Datapath.vhd) with PRESENT-specific accelerators
    PRESENT-80 (with accelerator) encryption application source file and simulator files
    PRESENT-80 (with accelerator) encryption object file
    
II. Limitations: Assembler and Simulator verified in Windows 8; Does not function correctly in LINUX

III. Quick Start Guide:

    See "Quick Start Guide" for SoftCore Project
    
    For assembly and simulation: Use presentkeyperm2.txt for source file, or pre-assembled presentkeyperm2obj.txt.  Use presentkeypermdata2.txt for simulation.
    
    For VHDL simulation, replace PROGLOAD.vhd and DATALOAD.vhd in SoftCore Project with PROGLOAD.vhd and DATALOAD.vhd from this project.  Update relevant generics and constants in loader_tb.vhd and loader.vhd.  Replace Datapath.vhd in SoftCore Project with Datapath.vhd from this project.
