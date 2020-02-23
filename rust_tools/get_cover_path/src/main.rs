use mpd::Client;
use pcre2::bytes::Regex;
use std::path::Path;
use std::str;
use std::fs;

fn ls(cap: &Path) -> std::io::Result<(String)> {
    let mut dir_vec: Vec<String> = Vec::new();
    for x in fs::read_dir(cap)? {
        let dir = x?.path();
        let path: &Path = dir.as_ref();
        let s = path.to_str().unwrap().to_string();
        dir_vec.push(s);
    }
    Ok(dir_vec.join(" "))
}

fn main() -> std::io::Result<()> {
    let mut conn = Client::connect("127.0.0.1:6600").expect("Can't connect to mpd server!");
    let current_file_path =
        Path::new(Box::leak(format!("/home/thomas/Music/{}", conn.currentsong().unwrap().unwrap().file).into_boxed_str()));

    let placeholder = Path::new("/home/thomas/placeholder.png");

    let current_album_path = current_file_path.parent().expect("Error getting parent path!");

    let re = Regex::new(r"(cover|folder)[0-9]?\.(jpg|jpeg|png|gif)").expect("Error creating re!"); // generate a pcre2 regex string.
    let mut cover_path = placeholder;
    if let Some(c) = re.captures(&ls(&current_album_path).unwrap().into_bytes()).expect("Rabbit hole a") { // captures string in the contents.
        let cover_file_slice = c[0].iter().cloned().collect::<Vec<u8>>(); // turn Capture -> Vec<u8>
        let cover_file = str::from_utf8(&cover_file_slice).expect("Error convert cover str slice!"); // Vec<u8> -> &[u8] -> String
        cover_path =
            Path::new(Box::leak(format!("{}/{}", current_album_path.to_string_lossy(), cover_file).into_boxed_str()));
    }// &ls(&current_album_path).unwrap().into_bytes() will return a reference to the bytestring of current dir file joined with space.
    print!("{}", cover_path.to_string_lossy());
    Ok(())
}
