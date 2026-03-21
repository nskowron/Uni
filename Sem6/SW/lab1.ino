byte letter_codes[] = {
  0b00110101, // a
  0b11000000, // b
  0b11001100, // c
  0b11000001, // d
  0b00010101, // e
  0b00001100, // f
  0b11110001, // g
  0b00000000, // h
  0b00000101, // i
  0b00111111, // j
  0b11001101, // k
  0b00110000, // l
  0b11110101, // m
  0b11000101, // n
  0b11111101, // o
  0b00111100, // p
  0b11110011, // q
  0b00110001, // r
  0b00000001, // s
  0b11010101, // t
  0b00001101, // u
  0b00000011, // v
  0b00111101, // w
  0b11000011, // x
  0b11001111, // y
  0b11110000  // z
};
int dot_delay = 150;
int dash_delay = 450;
int between_signals_delay = 150;
int between_letters_delay = 300;
char serialInput;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  Serial.println("Hello Arduino");
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  // put your main code here, to run repeatedly:
  while(Serial.available()) {
    serialInput = Serial.read();
    if(serialInput >= 'a' && serialInput <= 'z') { // is a letter
      blinkLetter(serialInput);
      delay(between_letters_delay);
    }
  }
}

void blinkLetter(char letter) {
  byte code = letter_codes[letter - 'a'];
  for(int i = 0; i < 4; i++) {
    int slide = (3 - i) * 2;
    byte signal = (code & (3 << slide)) >> slide;
    if(signal == 0b01) { // end of letter
      break;
    }
    digitalWrite(LED_BUILTIN, HIGH);
    switch(signal) {
      case 0b00: delay(dot_delay); break;
      case 0b11: delay(dash_delay); break;
    }
    digitalWrite(LED_BUILTIN, LOW);
    delay(between_signals_delay);
  }
}
