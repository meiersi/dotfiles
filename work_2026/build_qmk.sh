#! /bin/bash

set -eoux pipefail

zip="oryx/flow-1.zip"
source="oryx/zsa_voyager_mac-bunya_source"
keymap="$source/keymap.c"
config="$source/config.h"

ln -sf "$PWD/$source/" "keyboards/zsa/voyager/keymaps/flow"

rm -rf "$source"
unzip -od oryx "$zip"

# Sed it into shape
if  ! grep -q ' SS_DELAY(100)' "$keymap"; then
    echo "No occurrence of ' SS_DELAY(100)' in $keymap, exiting..."
    exit 1
fi
sed -i '' 's/ SS_DELAY(100)//g' "$keymap"


# # replace taps for BEC with SEND_STRING("because ");
# bec_key='SS_TAP(X_B) SS_TAP(X_E) SS_TAP(X_C)'
# if ! grep -q "$bec_key" "$keymap"; then
#     echo "Cannot find '$bec_key' in $keymap, exiting..."
#     exit 1
# fi
# sed -i "s/$bec_key/\"because \"/g" "$keymap"
#
# flow_keys=(U I J O)

# for key in "${flow_keys[@]}"; do
#     flow_key="SEND_STRING(SS_TAP(X_$key) SS_TAP(X_$key));"
#     if ! grep -q "$flow_key" "$keymap"; then
#         echo "Cannot find '$flow_key' in $keymap, exiting..."
#         exit 1
#     fi
#     sed -i "s/$flow_key/SEND_STRING(SS_TAP(X_$key)); record->event.pressed = false;/g" "$keymap"
# done

repeat_key='KC_F20'

if ! grep -q "$repeat_key" "$keymap"; then
    echo "Cannot find '$repeat_key' in $keymap, exiting..."
    exit 1
fi
sed -i '' "s/$repeat_key/QK_REPEAT_KEY/g" "$keymap"

# Append the feature flag to rules.mk
echo "REPEAT_KEY_ENABLE = yes" >> "$source/rules.mk"

# Replace the F21 key wit the alt repeat key
alt_repeat_key='KC_F21'
if ! grep -q "$alt_repeat_key" "$keymap"; then
    echo "Cannot find '$alt_repeat_key' in $keymap, exiting..."
    exit 1
fi
sed -i '' "s/$alt_repeat_key/QK_ALT_REPEAT_KEY/g" "$keymap"

sed -i '' "s/\(bool process_record_user.*\)/\
\1\n\
\/\/ Only alt-repeat once, and then just press space to avoid SFB on thumb\n\
if (record->event.pressed \&\& get_repeat_key_count() < -1) {\n\
    tap_code(KC_SPACE);\n\
    return false;\n\
}\n\
/" "$keymap"


# Repeat key mapping for the snth layout
echo "

#define HOME_B LT(4, KC_B)
#define HOME_N MT(MOD_LCTL, KC_N)
#define HOME_R MT(MOD_LALT, KC_R)
#define HOME_T LT(2, KC_T)
#define HOME_D MT(MOD_LGUI, KC_D)
#define HOME_P KC_P
#define HOME_Y KC_Y
#define HOME_H MT(MOD_LGUI, KC_H)
#define HOME_A MT(MOD_RSFT, KC_A)
#define HOME_E MT(MOD_LALT, KC_E)
#define HOME_I MT(MOD_RCTL, KC_I)
#define HOME_COMMA LT(3, KC_COMMA)


uint16_t get_alt_repeat_key_keycode_user(uint16_t keycode, uint8_t mods) {
    switch (keycode) {
        case KC_Y: return KC_I;
        case KC_U: return KC_A;
        case HOME_Y: return KC_E;
        case HOME_A: return KC_U;
        case HOME_E: return KC_O;
        case KC_QUOTE: return KC_E;

        case KC_MINUS: return KC_ENTER;
        case KC_GRAVE: return KC_ENTER;
        case KC_DOT: return KC_ENTER;
        case KC_COMMA: return KC_ENTER;
        case KC_SCLN: return KC_ENTER;
        case KC_SLASH: return KC_ENTER;
        case KC_BSLS: return KC_ENTER;

        // case KC_MINUS: return KC_A; // most frequent
        // case KC_GRAVE: return KC_A; // most frequent
        // case KC_DOT: return KC_A; // most frequent
        // case KC_COMMA: return KC_DOT; // Vim jump and repeat
        // case KC_SCLN: return KC_DOT; // Vim jump and repeat

        case HOME_B: return KC_I;

        case HOME_N: return KC_B;

        case KC_L: return KC_M;
        case KC_M: return KC_L;
        case HOME_H: return KC_L; // German

        case KC_K: return KC_L; // German

        case HOME_D: return KC_G;
        case HOME_T: return KC_D; // quite common in coding due to "std"
        case KC_G: return KC_T; // German

        case KC_W: return KC_S;
        // case KC_W: return KC_N;

        case KC_P: return KC_S;
        case HOME_P: return KC_P;
        case HOME_I: return KC_N;

        case KC_V: return KC_S;
        case KC_Q: return KC_S;
        case KC_X: return KC_C;
        // case KC_X: return KC_H;

        case HOME_COMMA: return KC_X;
        // case HOME_I: return KC_Z;
        case KC_Z: return KC_Y;

        case HOME_R: return KC_R; // easy repeat
        // case HOME_R: return KC_Z;

        // case US_UDIA: return KC_B;

    }
    return KC_TRNS;
}
"


# Configure the thumb keys without auto-repeat
echo "#define QUICK_TAP_TERM_PER_KEY" >> "$config"

echo "

uint16_t get_quick_tap_term(uint16_t keycode, keyrecord_t *record) {
    switch (keycode) {
        case MT(MOD_LSFT, KC_SPACE):
            return 0;
        case HOME_R:
            return 0;
        case HOME_E:
            return 0;
        default:
            return QUICK_TAP_TERM;
    }
}

" >> "$keymap"

make zsa/voyager:flow
