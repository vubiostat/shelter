

Keyring  :: String
Secret   :: String
Password :: String
Check    :: String(32)
Version  :: String

KeyName  :: String
Secret   :: String
KeyPair  :: (KeyName, Secret)

KeyringData :: [Version, Check, Password, [KeyPair]]
KeyringFile :: RDS [Version, Check, [(KeyName, sodium::data_encrypt Secret)]]

== Crypto Internal Routines ==

keyring_store           :: Keyring -> KeyringData -> IO ()
keyring_retrieve        :: Keyring -> Password -> IO KeyringEnv
keyring_assert_exists   :: Keyring -> Exception ()
keyring_assert_unlocked :: Keyring -> Exception ()

== Export Keyring Routines ==

keyring_exists :: KeyRing -> Bool              # checks if file exists

keyring_list   :: [(KeyRing, Int, Bool)]       # lists keyring, #secrets, locked
keyring_create :: KeyRing -> Password -> Bool  # creates memory and disk
keyring_locked :: KeyRing -> Bool              # checks if in memory
keyring_lock   :: KeyRing -> Bool              # deletes memory
keyring_unlock :: KeyRing -> Password -> Bool  # reads and loads into memory
keyring_delete :: KeyRing -> Bool              # deletes file and memory

== Specific Key/Secret Routines ==

Note: All call keyring_assert_unlocked

key_exists :: KeyRing -> Key -> Bool           # Does an entry exists
key_get    :: KeyRing -> Key -> Maybe Secret   # Return key secret or NULL
key_delete :: KeyRing -> Key -> Bool           # Delete a key
key_list   :: KeyRing -> [Key]                 # List of key names
key_set    :: KeyRing -> Key -> Secret -> Bool # Set a given key



