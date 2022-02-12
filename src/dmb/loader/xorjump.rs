pub fn xorjump(mut key: u8, data: &[u8]) -> Vec<u8> {
    data.iter().map(|byte| {
        let byte = byte ^ key;
        key = key.wrapping_add(9);
        byte
    }).collect()
}
