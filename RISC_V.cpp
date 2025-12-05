//na minha máquina máquina roda :)
#include <iostream>
#include <bitset>
#include <array>
#include <memory>
#include <vector>
#include <cctype>   // isprint
#include <iomanip>  // setw, hex
#include <sstream>  // stringstream

using namespace std;

#define SIZE 32

// --- Configurações VRAM / E/S programada
constexpr int VRAM_ADDR_START = 200; // endereço (byte) inicial da VRAM no espaço de dados
constexpr int VRAM_NUM_BYTES  = 20;  // quantos bytes da VRAM queremos mostrar
constexpr int DATA_MEMORY_WORDS = 1024; // número de palavras na memória de dados
constexpr int ES_N = 5; // imprimir VRAM a cada N instruções executadas

// ------------------ Helpers para decodificação ------------------
int get_opcode(int x) { return (x >> 0) & 0b1111111; }
int get_rd(int x)     { return (x >> 7) & 0b11111; }
int get_funct3(int x) { return (x >> 12) & 0b111; }
int get_rs1(int x)    { return (x >> 15) & 0b11111; }
int get_rs2(int x)    { return (x >> 20) & 0b11111; }
int get_shamt(int x)  { return (x >> 20) & 0b11111; }
int get_funct7(int x) { return (x >> 25) & 0b1111111; }
int get_imm_I(int x)  { return (x >> 20) & 0xFFF; } // imm[11:0]
int get_imm_S(int x)  {
    int imm4_0 = (x >> 7) & 0b11111;
    int imm11_5 = (x >> 25) & 0b1111111;
    return (imm11_5 << 5) | imm4_0;
}
int get_imm_B(int x) {
    // B-type: imm[12|10:5|4:1|11] (then <<1) -> build signed immediate
    int imm11 = (x >> 7) & 0b1;
    int imm4_1 = (x >> 8) & 0b1111;
    int imm10_5 = (x >> 25) & 0b111111;
    int imm12 = (x >> 31) & 0b1;
    int imm = (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1);
    return imm;
}
int get_imm_U(int x) {
    // U-type immediate is bits 31..12 shifted left 12
    return x & 0xFFFFF000;
}
int get_imm_J(int x) {
    // J-type: imm[20|10:1|11|19:12], then <<1
    int imm20 = (x >> 31) & 0b1;
    int imm10_1 = (x >> 21) & 0x3FF;
    int imm11 = (x >> 20) & 0b1;
    int imm19_12 = (x >> 12) & 0xFF;
    int imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
    return imm;
}

// sign-extend n-bit value to 32-bit signed int
static int sign_extend(int value, int bits) {
    int shift = 32 - bits;
    return (value << shift) >> shift; // arithmetic shift
}

char type_identifier(int x) {
    switch (get_opcode(x)) {
        case 0b0110011: return 'R';
        case 0b0010011: return 'I';
        case 0b1100111: return 'I';
        case 0b0001111: return 'I';
        case 0b1110011: return 'I';
        case 0b0000011: return 'I'; // loads
        case 0b0100011: return 'S'; // stores
        case 0b1100011: return 'B';
        case 0b0110111: return 'U';
        case 0b0010111: return 'U';
        case 0b1101111: return 'J';
        default: return '?';
    }
}

// ------------------ Instruction Register / Decoder (imprime campos) ------------------
class Instruction_Register {
public:
    // imprime campos binarios/decimais e detecta instrução (simples)
    void decoder(int x){
        char t = type_identifier(x);
        cout << "DECODE\n";
        cout << "Tipo: " << t << "\n";

        cout << "opcode:  " << bitset<7>(get_opcode(x)) << " (" << get_opcode(x) << ")\n";
        cout << "rd:      " << bitset<5>(get_rd(x)) << " (" << get_rd(x) << ")\n";
        cout << "funct3:  " << bitset<3>(get_funct3(x)) << " (" << get_funct3(x) << ")\n";
        cout << "rs1:     " << bitset<5>(get_rs1(x)) << " (" << get_rs1(x) << ")\n";
        cout << "rs2:     " << bitset<5>(get_rs2(x)) << " (" << get_rs2(x) << ")\n";
        cout << "funct7:  " << bitset<7>(get_funct7(x)) << " (" << get_funct7(x) << ")\n";

        // calcular imm32 de acordo com o tipo (sign-extend onde apropriado)
        int imm32 = 0;
        char tp = t;
        if (tp == 'I') {
            imm32 = sign_extend(get_imm_I(x), 12);
        } else if (tp == 'S') {
            imm32 = sign_extend(get_imm_S(x), 12);
        } else if (tp == 'B') {
            imm32 = sign_extend(get_imm_B(x), 13); // branch imm has 13 bits with sign
        } else if (tp == 'U') {
            imm32 = get_imm_U(x); // already upper bits
        } else if (tp == 'J') {
            imm32 = sign_extend(get_imm_J(x), 21);
        } else {
            imm32 = 0;
        }
        cout << "imm32:   " << bitset<32>(imm32) << " (" << imm32 << ")\n";

        // tentativa de identificar instrução (apenas as mais comuns)
        int opcode = get_opcode(x);
        int f3 = get_funct3(x);
        int f7 = get_funct7(x);
        int rs2 = get_rs2(x);

        if (tp == 'R') {
            if      (f3 == 0b001 && f7 == 0b00000) cout << "Instrucao detectada: SLL\n";
            else if (f3 == 0b101 && f7 == 0b00000) cout << "Instrucao detectada: SRL\n";
            else if (f3 == 0b101 && f7 == 0b01000) cout << "Instrucao detectada: SRA\n";
            else if (f3 == 0b000 && f7 == 0b00000) cout << "Instrucao detectada: ADD\n";
            else if (f3 == 0b000 && f7 == 0b01000) cout << "Instrucao detectada: SUB\n";
            else if (f3 == 0b100 && f7 == 0b00000) cout << "Instrucao detectada: XOR\n";
            else if (f3 == 0b110 && f7 == 0b00000) cout << "Instrucao detectada: OR\n";
            else if (f3 == 0b111 && f7 == 0b00000) cout << "Instrucao detectada: AND\n";
            else if (f3 == 0b010 && f7 == 0b00000) cout << "Instrucao detectada: SLT\n";
            else if (f3 == 0b011 && f7 == 0b00000) cout << "Instrucao detectada: SLTU\n";
            else cout << "Instrucao R-type (outro)\n";
        }
        else if (tp == 'I') {
            if      (opcode == 0b0010011 && f3 == 0b001 && f7 == 0b00000) cout << "Instrucao detectada: SLLI\n";
            else if (opcode == 0b0010011 && (f3 == 0b101 && f7 == 0b00000)) cout << "Instrucao detectada: SRLI\n";
            else if (opcode == 0b0010011 && (f3 == 0b101 && f7 == 0b01000)) cout << "Instrucao detectada: SRAI\n";
            else if (opcode == 0b0010011 && f3 == 0b000) cout << "Instrucao detectada: ADDI\n";
            else if (opcode == 0b0010011 && f3 == 0b100) cout << "Instrucao detectada: XORI\n";
            else if (opcode == 0b0010011 && f3 == 0b110) cout << "Instrucao detectada: ORI\n";
            else if (opcode == 0b0010011 && f3 == 0b111) cout << "Instrucao detectada: ANDI\n";
            else if (opcode == 0b0010011 && f3 == 0b010) cout << "Instrucao detectada: SLTI\n";
            else if (opcode == 0b0010011 && f3 == 0b011) cout << "Instrucao detectada: SLTIU\n";
            else if (opcode == 0b1100111 && f3 == 0b000) cout << "Instrucao detectada: JALR\n";
            else if (opcode == 0b0001111 && f3 == 0b000) cout << "Instrucao detectada: FENCE\n";
            else if (opcode == 0b0001111 && f3 == 0b001) cout << "Instrucao detectada: FENCE.I\n";
            else if (opcode == 0b1110011 && rs2 == 0b00000) cout << "Instrucao detectada: ECALL\n";
            else if (opcode == 0b1110011 && rs2 == 0b00001) cout << "Instrucao detectada: EBREAK\n";
            else cout << "Instrucao I-type (outro)\n";
        }
        else if (tp == 'S') {
            if (f3==0b010) cout << "Instrucao detectada: SW\n";
            else cout << "Instrucao S-type (outro)\n";
        }
        else if (tp == 'B') {
            if      (f3==0b000) cout << "Instrucao detectada: BEQ\n";
            else if (f3==0b001) cout << "Instrucao detectada: BNE\n";
            else if (f3==0b100) cout << "Instrucao detectada: BLT\n";
            else if (f3==0b101) cout << "Instrucao detectada: BGE\n";
            else if (f3==0b110) cout << "Instrucao detectada: BLTU\n";
            else if (f3==0b111) cout << "Instrucao detectada: BGEU\n";
            else cout << "Instrucao B-type (outro)\n";
        }
        else if (tp == 'U') {
            if      (opcode == 0b0110111) cout << "Instrucao detectada: LUI\n";
            else if (opcode == 0b0010111) cout << "Instrucao detectada: AUIPC\n";
            else cout << "Instrucao U-type (outro)\n";
        }
        else if (tp == 'J') {
            cout << "Instrucao detectada: JAL\n";
        }
        else {
            cout << "Instrucao desconhecida\n";
        }
        cout << endl;
    }
};

// --- barramento (para simular mem_read/mem_write)
struct Bus {
    int address = 0;
    int write_data = 0;
    int read_data = 0;
    bool mem_read = false;
    bool mem_write = false;
};

// ------------------ CPU ------------------
class CPU {
private:
    array<int, 32> registers;
    unique_ptr<int> PC;
    Instruction_Register IR;
    vector<int> memoria;      // mem. de instrucoes (cada entry = 32-bit instrução)
    vector<int> data_memory;  // mem. de dados (cada entry = 32-bit word)
    Bus bus;

    int executed_instructions = 0; // contador para E/S programada

public:
    CPU() : PC(make_unique<int>(0)) {
        for (int &r : registers) r = 0;
        data_memory.resize(DATA_MEMORY_WORDS, 0);
    }

    void loadProgram(const vector<int>& program) {
        memoria = program;
        *PC = 0;
    }

    // setar registrador manualmente (útil para testes)
    void setRegister(int reg, int value) {
        if (reg != 0 && reg >= 0 && reg < 32)
            registers[reg] = value;
    }

    // --- FETCH
    int fetch() {
        if (*PC < 0 || *PC >= (int)memoria.size()) {
            cout << "PC fora da memoria, encerrando execucao\n";
            return -1;
        }
        int instr = memoria[*PC];

        // --- ASCII FETCH PANEL ---
        {
            // instr hex string
            stringstream ss;
            ss << "0x" << hex << instr << dec;
            string instr_hex = ss.str();

            cout << "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
            cout << "┃                    FETCH                     ┃\n";
            cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
            cout << "┃ PC:        " << *PC << "\n";
            cout << "┃ Instrucao: " << instr_hex << "\n";
            cout << "┃ Binario:   " << bitset<32>(instr) << "\n";
            cout << "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n";
        }

        return instr;
    }

    // --- DECODE (agora imprime painel ASCII + mantém o decoder original)
    void decode(int instr) {
        // prepare some fields for the panel
        int opcode = get_opcode(instr);
        int rd = get_rd(instr);
        int rs1 = get_rs1(instr);
        int rs2 = get_rs2(instr);
        int funct3 = get_funct3(instr);
        int funct7 = get_funct7(instr);
        int imm = 0;
        char tp = type_identifier(instr);
        if (tp == 'I') imm = sign_extend(get_imm_I(instr), 12);
        else if (tp == 'S') imm = sign_extend(get_imm_S(instr), 12);
        else if (tp == 'B') imm = sign_extend(get_imm_B(instr), 13);
        else if (tp == 'U') imm = get_imm_U(instr);
        else if (tp == 'J') imm = sign_extend(get_imm_J(instr), 21);

        cout << "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
        cout << "┃                    DECODE                    ┃\n";
        cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
        cout << "┃ Opcode:   " << opcode << "\n";
        cout << "┃ rd:       " << rd << "\n";
        cout << "┃ rs1:      " << rs1 << "\n";
        cout << "┃ rs2:      " << rs2 << "\n";
        cout << "┃ funct3:   " << funct3 << "\n";
        cout << "┃ funct7:   " << funct7 << "\n";
        cout << "┃ Imediato: " << imm << "\n";
        cout << "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n";

        // keep original verbose decoder output
        IR.decoder(instr);
    }

    // --- acesso via barramento: load word (endereço em bytes)
    int lw_bus(int address) {
        bus.address = address;
        bus.mem_read = true;
        bus.mem_write = false;

        // BUS panel (read)
        cout << "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
        cout << "┃                MEMORY / BUS                  ┃\n";
        cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
        cout << "┃ Operacao:     LOAD (LW)                      ┃\n";
        cout << "┃ Endereco:     " << address << "\n";
        cout << "┃ Dado escrito: " << bus.write_data << "\n";
        cout << "┃ Dado lido:    (em breve)                    ┃\n";
        cout << "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n";

        if (address % 4 != 0) {
            cout << "Erro: endereco desalinhado para LW (" << address << ")\n";
            return 0;
        }
        int idx = address / 4;
        if (idx < 0 || idx >= (int)data_memory.size()) {
            cout << "Erro: acesso de leitura fora da memoria (" << address << ")\n";
            return 0;
        }
        bus.read_data = data_memory[idx];
        cout << "[BUS] LW from address " << address << " (word idx " << idx << ") -> " << bus.read_data << "\n";
        return bus.read_data;
    }

    // --- acesso via barramento: store word
    void sw_bus(int address, int value){
        bus.address = address;
        bus.write_data = value;
        bus.mem_write = true;
        bus.mem_read = false;

        // BUS panel (write)
        cout << "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
        cout << "┃                MEMORY / BUS                  ┃\n";
        cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
        cout << "┃ Operacao:     STORE (SW)                     ┃\n";
        cout << "┃ Endereco:     " << address << "\n";
        cout << "┃ Dado escrito: " << value << "\n";
        cout << "┃ Dado lido:    (n/a)                          \n";
        cout << "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n";

        if (address % 4 != 0) {
            cout << "Erro: endereco desalinhado para SW (" << address << ")\n";
            return;
        }
        int idx = address / 4;
        if (idx < 0 || idx >= (int)data_memory.size()) {
            cout << "Erro: acesso de escrita fora da memoria (" << address << ")\n";
            return;
        }
        data_memory[idx] = value;
        cout << "BUS SW para endereco " << address << " (indice " << idx << ") <- " << value << "\n";
    }

    // --- Imprime VRAM (bytes) como ASCII + hex -- mostra VRAM_NUM_BYTES bytes a partir de VRAM_ADDR_START
    void print_vram_ascii() {
        cout << "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
        cout << "┃                     VRAM                     ┃\n";
        cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
        cout << "┃ Endereco │ Hex  │ ASCII                      ┃\n";
        cout << "┃──────────┼──────┼────────────────────────────┃\n";
        // print single line: addr : 0xhh ('c')
        for (int i = 0; i < VRAM_NUM_BYTES; ++i) {
            int addr = VRAM_ADDR_START + i;
            int word_idx = addr / 4;
            int byte_offset = addr % 4;
            if (word_idx < 0 || word_idx >= (int)data_memory.size()) {
                cout << "┃ " << setw(8) << addr << " │ " << "  --" << " │ " << "out-of-range"
                     << "                       ┃\n";
                continue;
            }
            int word = data_memory[word_idx];
            // extract byte (little-endian assumption - LSB at lowest address)
            unsigned char byte = (word >> (8 * byte_offset)) & 0xFF;
            char ch = (isprint(byte) ? (char)byte : '.');

            // print formatted row
            cout << "┃ " << setw(8) << addr
                 << " │ 0x" << hex << setw(2) << setfill('0') << (int)byte << dec << setfill(' ')
                 << " │ '" << ch << "'" 
                 << "                       ┃\n";
        }
        cout << "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n";
    }

    // --- EXECUTE (faz as operações e acessos à memória)
    void execute_instruction(int instr) {
        // EXECUTE panel header
        {
            int opcode = get_opcode(instr);
            char tp = type_identifier(instr);
            cout << "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
            cout << "┃                   EXECUTE                    ┃\n";
            cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
            cout << "┃ Opcode:        " << opcode << " (type " << tp << ")\n";
            cout << "┃ Instr Bin:     " << bitset<32>(instr) << "\n";
            cout << "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n";
        }

        cout << "\nEXECUTE";
        int opcode = get_opcode(instr);

        // R-TYPE (opcode 0110011)
        if (opcode == 0b0110011) {
            int rd = get_rd(instr);
            int rs1 = get_rs1(instr);
            int rs2 = get_rs2(instr);
            int f3= get_funct3(instr);
            int f7 = get_funct7(instr);

            // SLL
            if (f3 == 0b001 && f7 == 0b00000) {
                cout << "\nEXECUTANDO SLL\n";
                if (rd != 0) registers[rd] = registers[rs1] << registers[rs2];
            }
            // SRL
            if (f3 == 0b001 && f7 == 0b00000) {
                cout << "\nEXECUTANDO SRL\n";
                if (rd != 0) registers[rd] = registers[rs1] >> registers[rs2];
            }
            // SRA
            if (f3 == 0b001 && f7 == 0b00000) {
                cout << "\nEXECUTANDO SRA\n";
                if (rd != 0) registers[rd] = static_cast<int32_t>(registers[rs1]) >> registers[rs2];
            }
            // ADD
            else if (f3== 0b000 && f7 == 0b0000000) {
                cout << "\nEXECUTANDO ADD\n";
                if (rd != 0) registers[rd] = registers[rs1] + registers[rs2];
            }
            // SUB
            else if (f3== 0b000 && f7 == 0b0100000) {
                cout << "\nEXECUTANDO SUB\n";
                if (rd != 0) registers[rd] = registers[rs1] - registers[rs2];
            }
            // AND
            else if (f3== 0b111) {
                cout << "\nEXECUTANDO AND\n";
                if (rd != 0) registers[rd] = registers[rs1] & registers[rs2];
            }
            // OR
            else if (f3== 0b110) {
                cout << "\nEXECUTANDO OR\n";
                if (rd != 0) registers[rd] = registers[rs1] | registers[rs2];
            }
            // XOR
            else if (f3== 0b100) {
                cout << "\nEXECUTANDO XOR\n";
                if (rd != 0) registers[rd] = registers[rs1] ^ registers[rs2];
            }
            // SLT
            else if (f3== 0b010) {
                cout << "\nEXECUTANDO SLT\n";
                if (rd != 0) registers[rd] = (registers[rs1] < registers[rs2]) ? 1 : 0;
            }
            else {
                cout << "R-type nao implementado (funct3=" << f3<< " funct7=" << f7 << ")\n";
            }
        }

        // I-TYPE OP-IMM (0010011)
        else if (opcode == 0b0010011) {
            int rd = get_rd(instr);
            int rs1 = get_rs1(instr);
            int f3= get_funct3(instr);
            int f7 = get_funct7(instr);
            int imm12 = sign_extend(get_imm_I(instr), 12);

            //SLLI
            if (f3 == 0b001 && f7 == 0b00000) {
                cout << "\nEXECUTANDO SLLI\n";
                if (rd != 0) registers[rd] =  registers[rs1] << imm12;
            }
            //SRLI
            else if (f3 == 0b101 && f7 == 0b00000) {
                cout << "\nEXECUTANDO SRLI\n";
                if (rd != 0) registers[rd] = registers[rs1] >> imm12;
            }
            //SRAI
            else if (f3 == 0b101 && f7 == 0b01000) {
                cout << "\nEXECUTANDO SRAI\n";
                if (rd != 0) registers[rd] = static_cast<int32_t>(registers[rs1]) >> imm12;
            }
            //ADDI
            else if (f3== 0b000) {
                cout << "\nEXECUTANDO ADDI\n";
                if (rd != 0)  registers[rd] = registers[rs1] + imm12;
            }
            //XORI
            else if (f3== 0b100) {
                cout << "\nEXECUTANDO XORI\n";
                if (rd != 0) registers[rd] = registers[rs1] ^ imm12;
            }
            //ANDI
            else if (f3== 0b111) {
                cout << "\nEXECUTANDO ANDI\n";
                if (rd != 0) registers[rd] = registers[rs1] & imm12;
            }
            //ORI
            else if (f3== 0b110) {
                cout << "\nEXECUTANDO ORI\n";
                if (rd != 0) registers[rd] = registers[rs1] | imm12;
            }
            //SLTI
            else if (f3== 0b010) {
                cout << "\nEXECUTANDO SLTI\n";
                if (rd != 0) registers[rd] = (registers[rs1] < imm12) ? 1 : 0;
            }
            else {
                cout << "I-type OP-IMM nao implementado (funct3=" << f3<< ")\n";
            }
        }
        // LOAD (LW) opcode 0000011
        else if (opcode == 0b0000011) {
            int rd = get_rd(instr);
            int rs1 = get_rs1(instr);
            int imm12 = sign_extend(get_imm_I(instr), 12);
            int addr = registers[rs1] + imm12;
            cout << "EXECUTANDO LW -> endereco: " << addr << "\n";
            int value = lw_bus(addr);
            if (rd != 0) registers[rd] = value;
        }
        // STORE (SW) opcode 0100011
        else if (opcode == 0b0100011) {
            int rs1 = get_rs1(instr);
            int rs2 = get_rs2(instr);
            int imm12 = sign_extend(get_imm_S(instr), 12);
            int addr = registers[rs1] + imm12;
            cout << "EXECUTANDO SW -> endereco: " << addr << "\n";
            sw_bus(addr, registers[rs2]);
        }
        // B-Type
        else if (opcode == 0b1100011) {
            int rs1 = get_rs1(instr);
            int rs2 = get_rs2(instr);
            int f3 = get_funct3(instr);

            // BEQ
            if (f3 == 0b000) {
                cout << "\nEXECUTANDO BEQ\n";
                if ((registers[rs1] != 0) && (registers[rs2] != 0)) {
                    if (registers[rs1] == registers[rs2]) cout << "\nIGUAL\n";
                }
            }
            // BNE
            else if (f3 == 0b001) {
                cout << "\nEXECUTANDO BNE\n";
                if ((registers[rs1] != 0) && (registers[rs2] != 0)) {
                    if (registers[rs1] != registers[rs2]) cout << "\nDIFERENTE\n";
                }
            }
            // BLT
            else if (f3 == 0b100) {
                cout << "\nEXECUTANDO BLT\n";
                if ((registers[rs1] != 0) && (registers[rs2] != 0)) {
                    if (registers[rs1] < registers[rs2]) cout << "\nMENOR COM SINAL\n";
                }
            }
            // BGE
            else if (f3 == 0b101) {
                cout << "\nEXECUTANDO BGE\n";
                if ((registers[rs1] != 0) && (registers[rs2] != 0)) {
                    if (registers[rs1] >= registers[rs2]) cout << "\nMAIOR OU IGUAL COM SINAL\n";
                }
            }
            // BLTU
            else if (f3 == 0b110) {
                cout << "\nEXECUTANDO BLTU\n";
                if ((registers[rs1] != 0) && (registers[rs2] != 0)) {
                    if (registers[rs1] < registers[rs2]) cout << "\nMENOR SEM SINAL\n";
                }
            }
            // BGEU
            else if (f3 == 0b111) {
                cout << "\nEXECUTANDO BGEU\n";
                if ((registers[rs1] != 0) && (registers[rs2] != 0)) {
                    if (registers[rs1] >= registers[rs2]) cout << "\nMAIOR OU IGUAL SEM SINAL\n";
                }
            }
            else {
                cout << "INSTRUCAO DO TIPO B NAO ENCONTRADA";
            }
        }
        else if (opcode == 0b0110111) {
            int rd = get_rd(instr);
            int imm20 = get_imm_U(instr);
            cout << "\nEXECUTANDO LUI\n";
            if (registers[rd] != 0) {
                registers[rd] = imm20 << 12;
            }
        }
        else {
            cout << "Opcode nao implementado na execucao: " << opcode << "\n";
        }
        // proteção do registrador x0
        registers[0] = 0;
    }

    // --- WRITEBACK (apenas imprime estado parcial dos registradores)
    void write_back(int) {
        cout << "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n";
        cout << "┃                  WRITEBACK                   ┃\n";
        cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
        cout << "┃ Registradores (x0..x15)                      ┃\n";
        cout << "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n";
        cout << "┃ ";
        for (int i = 0; i < 16; ++i) {
            cout << "x" << i << "=" << registers[i] << (i==15? "" : " ");
        }
        cout << "\n";
        cout << "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n\n";

        // keep original verbose writeback output (for compatibility)
        cout << "\n[WRITEBACK]\n";
        cout << "Registradores (x0..x15): ";
        for (int i = 0; i < 16; ++i) {
            cout << "x" << i << "=" << registers[i] << (i==15? "\n" : " ");
        }
        cout << endl;
    }

    // --- Inicializa com os registradores zerados
    void reset_register(int) {
        for (int i = 0; i < 16; i++) {
            registers[i] = 0;
        }
    }

    void advancePC() { (*PC)++; }

    // --- step: um ciclo fetch-decode-execute-writeback; também faz a E/S programada
    bool step() {
        int instr = fetch();
        if (instr == -1) return false;

        decode(instr);
        execute_instruction(instr);

        // conta instrução executada e, se atingir N, chama E/S (VRAM dump)
        executed_instructions++;
        if (executed_instructions % ES_N == 0) {
            cout << "\n--- E/S Programada: depois de " << executed_instructions
                 << " instrucoes, imprimindo VRAM ---\n";
            print_vram_ascii();
        }

        write_back(instr);
        advancePC();

        return true;
    }

    void run() {
        cout << "\nINICIANDO EXECUCAO\n";
        while (step()) { }
        cout << "\nEXECUCAO FINALIZADA\n";
    }
};

// ------------------ MAIN: exemplo de uso ------------------
int main() {
    CPU cpu;

    // --- Inicializa alguns registradores (teste)
    cpu.setRegister(0, 0);    // x0 sempre 0 (proteção)
    // vamos usar x5 como ponteiro/base para VRAM
    cpu.setRegister(5, VRAM_ADDR_START); // x5 = VRAM base (em bytes)

    cpu.reset_register(0);

    // n1 = addi x1 x1 5
    int n1 = 0b000000000101'00001'000'00001'0010011;
    cpu.execute_instruction(n1); // x1 = 5
    // n2 = addi x2 x2 5
    int n2 = 0b000000000101'00010'000'00010'0010011;
    cpu.execute_instruction(n2); // x2 = 5
    // n3 = addi x3 x3 6
    int n3 = 0b000000000110'00011'000'00011'0010011;
    cpu.execute_instruction(n3); // x3 = 6

    /**
    // add x4, x1, x2
    int n6 = 0b00000'00'00010'00001'000'00100'0110011;
    cpu.execute_instruction(n6); // x4 = x1 + x2
    // add x5, x2, x3
    int n7 = 0b00000'00'00011'00010'000'00101'0110011;
    cpu.execute_instruction(n7); // x5 = x2 + x3



    // n8 = sub x6 x5 x8
    int n8 = 0b01000'00'01000'00101'000'00110'0110011;
    cpu.execute_instruction(n8); // x6 = x5 - x8



    // n9 = addi x8 x8 1
    int n9 = 0b000000000001'01000'000'01000'0010011;
    cpu.execute_instruction(n9); // x8 = 1

    // n10 = slt x4 x8 x3
    int n10 = 0b00000'00'00011'01000'010'00100'0110011;
    cpu.execute_instruction(n10); // x4 = 3

    // n11 = beq x1 x2
    int n11 = 0b0'000000'00010'00001'101'0000'0'1100011;
    cpu.execute_instruction(n11);

    // n12 = bne x2 x3
    int n12 = 0b0'000000'00011'00010'100'0000'0'1100011;
    cpu.execute_instruction(n12);

    */

    // n8 = sub x4 x2 x3
    int n8 = 0b01000'00'00011'00010'000'00100'0110011;
    cpu.execute_instruction(n8); // x4 = x2 - x3

    // n11 = beq x1 x2
    int n9 = 0b0'000000'00100'00001'111'0000'0'1100011;
    cpu.execute_instruction(n9);

    // n12 = bne x2 x3
    int n12 = 0b0'000000'00010'00100'110'0000'0'1100011;
    cpu.execute_instruction(n12);

    cpu.write_back(0);

    
    return  0;
}

