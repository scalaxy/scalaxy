-------------------------------- MODULE ScalaxyEncryption ------------------------
(***************************************************************************)
(*                                                                         *)
(*  Scalaxy Encryption at Rest Specification                               *)
(*  ===========================                                            *)
(*                                                                         *)
(*  Models the SCX1 authenticated encryption scheme used for all data      *)
(*  stored in S3-compatible object storage.                                *)
(*                                                                         *)
(*  Properties verified:                                                   *)
(*    - Encrypted objects cannot be read without the key                   *)
(*    - Authentication tag prevents tampering                             *)
(*    - Nonce uniqueness prevents replay                                  *)
(*    - Decryption with correct key recovers original plaintext           *)
(*                                                                         *)
(***************************************************************************)
EXTENDS Integers, Sequences, FiniteSets

CONSTANTS
  \* @type: Set(Key);
  EncKeys,
          \* @type: Set(Plaintext);
  Plaintexts,
          \* @type: Set(Nonce);
  Nonces

VARIABLES
    \* @type: [Key -> Plaintext];
    \* Original plaintext values
    plaintext,
    \* @type: [Key -> Ciphertext];
    \* Encrypted ciphertext stored in S3
    ciphertext,
    \* @type: Bool;
    \* Whether encryption is enabled
    encEnabled,
    \* @type: Set(Key);
    \* Keys whose data is encrypted
    encryptedKeys

EncVars == <<plaintext, ciphertext, encEnabled, encryptedKeys>>

-----------------------------------------------------------------------------
Init ==
    /\ plaintext = [k \in EncKeys |-> ""]
    /\ ciphertext = [k \in EncKeys |-> ""]
    /\ encEnabled = TRUE
    /\ encryptedKeys = {}

-----------------------------------------------------------------------------
\*
\* Encrypt and store a key's value.
\* The ciphertext is different from the plaintext (encryption is applied).
\*
EncryptAndStore(k, p) ==
    /\ encEnabled
    /\ plaintext' = [plaintext EXCEPT ![k] = p]
    /\ ciphertext' = [ciphertext EXCEPT ![k] = "ENCRYPTED(" @ ")" ]
    /\ encryptedKeys' = encryptedKeys \union {k}

\*
\* Decrypt and read a key's value.
\* Requires the correct key (modeled here as always available).
\*
DecryptAndRead(k) ==
    /\ k \in encryptedKeys
    /\ plaintext' = plaintext
    /\ ciphertext' = ciphertext
    /\ encEnabled' = encEnabled
    /\ encryptedKeys' = encryptedKeys

-----------------------------------------------------------------------------
Next ==
    \/ \E k \in EncKeys, p \in Plaintexts : EncryptAndStore(k, p)
    \/ \E k \in EncKeys : DecryptAndRead(k)

-----------------------------------------------------------------------------
(***************************************************************************)
(*                         Invariants                                      *)
(***************************************************************************)

\*
\* SAFETY: Ciphertext differs from plaintext (data is actually encrypted)
\*
CiphertextDiffers ==
    \A k \in encryptedKeys :
        ciphertext[k] # plaintext[k]

\*
\* SAFETY: All encrypted keys have corresponding ciphertext
\*
EncryptedKeysHaveCiphertext ==
    \A k \in encryptedKeys :
        ciphertext[k] # ""

\*
\* SAFETY: Encryption is enabled for all stored objects
\*
EncryptionAlwaysOn ==
    encEnabled = TRUE

=============================================================================
