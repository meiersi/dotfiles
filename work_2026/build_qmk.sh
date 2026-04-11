#! /bin/bash

set -eoux pipefail

zip="oryx/flow-1.zip"
source="oryx/zsa_voyager_qwerty-with-repeat-key_source"
keymap="$source/keymap.c"
config="$source/config.h"

ln -sf "$PWD/$source/" "keyboards/zsa/voyager/keymaps/flow"


unzip -od oryx "$zip"

# Sed it into shape
if  ! grep -q ' SS_DELAY(100)' "$keymap"; then
    echo "No occurrence of ' SS_DELAY(100)' in $keymap, exiting..."
    exit 1
fi
sed -i 's/ SS_DELAY(100)//g' "$keymap"


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
sed -i "s/$repeat_key/QK_REPEAT_KEY/g" "$keymap"

# Append the feature flag to rules.mk
echo "REPEAT_KEY_ENABLE = yes" >> "$source/rules.mk"

# Replace the F21 key wit the alt repeat key
alt_repeat_key='KC_F21'
if ! grep -q "$alt_repeat_key" "$keymap"; then
    echo "Cannot find '$alt_repeat_key' in $keymap, exiting..."
    exit 1
fi
sed -i "s/$alt_repeat_key/QK_ALT_REPEAT_KEY/g" "$keymap"

sed -i "s/\(bool process_record_user.*\)/\
\1\n\
\/\/ Only alt-repeat once, and then just press space to avoid SFB on thumb\n\
if (record->event.pressed \&\& get_repeat_key_count() < -1) {\n\
    tap_code(KC_SPACE);\n\
    return false;\n\
}\n\
/" "$keymap"

if false; then
    # Append the alt repeat key function to the keymap
    echo "

    #define HOME_B KC_B
    #define HOME_N LT(8,KC_N)
    #define HOME_R MT(MOD_LALT, KC_R)
    #define HOME_T MT(MOD_LSFT, KC_T)
    #define HOME_S MT(MOD_LCTL, KC_S)
    #define HOME_G MT(MOD_LGUI, KC_G)
    // #define HOME_Y MT(MOD_RGUI, KC_Y)
    #define HOME_H MT(MOD_RCTL, KC_H)
    #define HOME_A LT(1,KC_A)
    #define HOME_E MT(MOD_LALT, KC_E)
    #define HOME_I LT(3,KC_I)
    // #define HOME_QUOTE KC_QUOTE
    #define HOME_QUOTE MT(MOD_RGUI, KC_QUOTE)

    // For i swapped I and A for less lat stretch
    // #define HOME_A LT(3,KC_A)
    // #define HOME_I LT(1,KC_I)

    uint16_t get_alt_repeat_key_keycode_user(uint16_t keycode, uint8_t mods) {
        switch (keycode) {
        case HOME_B: return KC_J; // bad stretch and dsfb
        // case HOME_B: return KC_Y;
        // case HOME_B: return KC_S;
        case HOME_N: return KC_B;
        // case HOME_N: return KC_J;
        // case HOME_N: return KC_Z;
        case HOME_R: return KC_L;
        case HOME_T: return KC_M;
        case HOME_S: return KC_C;
        case HOME_G: return KC_S;
        // case HOME_Y: return KC_P;
        // case KC_Y: return KC_QUOTE;
        case KC_Y: return KC_P;
        case HOME_H: return KC_Y;
        // case HOME_A: return KC_U;
        // case HOME_A: return KC_Y; // lower lateral stretch
        case HOME_A: return KC_O;
        // A reserved for a-umlaut
        case HOME_E: return KC_U;
        // case HOME_I: return KC_O;
        // case HOME_I: return KC_E;  // bad redirect
        case HOME_I: return KC_DOT;
        case KC_L: return KC_R;
        case KC_D: return KC_M;
        case KC_W: return KC_S;
        case KC_V: return KC_S;
        case KC_G: return KC_S;
        case KC_Z: return KC_Y;
        case KC_C: return KC_S;
        case KC_M: return KC_T;
        case KC_K: return KC_Y;
        case KC_F: return KC_P;
        // case KC_O: return KC_I;
        case KC_O: return KC_A;
        case KC_U: return KC_E;
        case KC_P: return KC_H;
        // case KC_P: return KC_Y;
        case KC_DOT: return KC_I;
        // case KC_DOT: return KC_SLASH;
        case KC_MINUS: return KC_E;
        // case KC_MINUS: return KC_I;
        case KC_0: return KC_COLON;
        case KC_1: return KC_4;
        case KC_2: return KC_5;
        case KC_3: return KC_6;
        case KC_4: return KC_7;
        case KC_5: return KC_8;
        case KC_6: return KC_9;
        case KC_7: return KC_4;
        case KC_8: return KC_5;
        case KC_9: return KC_6;
        }
        return KC_TRNS;
    }

    " >> "$keymap"

fi

if true; then
    # Repeat key mapping for the snth layout

    echo "

    #define HOME_I MT(MOD_LALT, KC_I)
    // #define HOME_E MT(MOD_LSFT, KC_E)
    #define HOME_E LT(2,KC_E)
    #define HOME_C LT(4,KC_C)
    #define HOME_A MT(MOD_LCTL, KC_A)
    #define HOME_H MT(MOD_RCTL, KC_H)
    #define HOME_T MT(MOD_RSFT, KC_T)
    #define HOME_N MT(MOD_LALT, KC_N)
    #define HOME_S LT(3,KC_S)
    #define HOME_R LT(1,KC_R)
    #define HOME_DOT KC_DOT
    // #define HOME_DOT MT(MOD_RGUI, KC_DOT)
    #define HOME_B KC_B
    // #define HOME_B TD(DANCE_0)
    // #define HOME_O TD(DANCE_0)
    #define HOME_O KC_O
    #define HOME_D KC_D
    // #define HOME_D TD(DANCE_1)
    // #define HOME_F TD(DANCE_1)
    #define HOME_F KC_F


    uint16_t get_alt_repeat_key_keycode_user(uint16_t keycode, uint8_t mods) {
        switch (keycode) {
            case KC_Y: return KC_I;
            case KC_U: return KC_A;
            case HOME_O: return KC_E;
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

            case HOME_C: return KC_B;

            case KC_L: return KC_M;
            case KC_M: return KC_L;
            case HOME_H: return KC_L; // German

            case KC_K: return KC_L; // German

            case HOME_D: return KC_G;
            case HOME_T: return KC_D; // quite common in coding due to "std"
            case KC_G: return KC_T; // German

            case KC_W: return KC_S;
            // case KC_W: return KC_N;

            case HOME_N: return KC_F;
            case KC_P: return KC_S;
            case HOME_S: return KC_P;
            case HOME_F: return KC_N;

            case KC_V: return KC_S;
            case KC_Q: return KC_S;
            case KC_X: return KC_C;
            // case KC_X: return KC_H;

            case HOME_I: return KC_X;
            // case HOME_I: return KC_Z;
            case KC_Z: return KC_Y;

            case HOME_R: return KC_R; // easy repeat
            // case HOME_R: return KC_Z;

            case US_UDIA: return KC_B;

        }
        return KC_TRNS;
    }

    " >> "$keymap"
fi

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
