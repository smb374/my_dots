extern crate image;
extern crate mpd;
extern crate pcre2;

use std::fs::{ self, File, OpenOptions };
use std::path::Path;
use std::str;

use mpd::{ Client, Song };
use pcre2::bytes::Regex;
use image::imageops::FilterType;

fn convert(cp: &Path, op: &Path) -> Result<(), ()> {
    let im = image::open(cp).unwrap();
    let resized = im.resize(320, 160, FilterType::Gaussian);
    match resized.save(op) {
        Ok(_) => Ok(()),
        Err(err) => panic!("Can't save image! Error: {}", err),
    }
}

fn process(cp: &Path, op: &Path, p: &Path) -> std::io::Result<()> {
    match OpenOptions::new().read(true).write(false).open(cp) {
        Ok(_) => {
            convert(&cp, &op).expect("Can't convert!");
        },
        Err(_) => {
            to_placeholder(&p, &op)?;
        },
    }
    Ok(())
}

fn to_placeholder(p: &Path, o: &Path) -> std::io::Result<()> {
    let data = fs::read(p)?;
    if OpenOptions::new().read(true).write(false).open(o).is_err() {
        File::create(o)?;
    }
    fs::write(o, data)?;
    Ok(())
}

fn ls(cap: &Path) -> std::io::Result<String> {
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
    let current_song: Song = conn.currentsong().unwrap().unwrap();
    let current_file_path =
        Path::new(Box::leak(format!("/home/thomas/Music/{}", current_song.file).into_boxed_str()));

    let out_name = "cover_mpd.png";
    let out_path = Path::new(
        Box::leak(
            format!("/tmp/{}", out_name).into_boxed_str()
        )
    ); // use Box::leak() to leak memory from String to get &str

    let placeholder = Path::new("/home/thomas/placeholder.png");

    let current_album_path = current_file_path.parent().expect("Error getting parent path!");

    let re = Regex::new(r"(cover|folder)[0-9]?\.(jpg|jpeg|png|gif)").expect("Error creating re!"); // generate a pcre2 regex string.
    let mut cover_path = placeholder;
    if let Some(c) = re.captures(&ls(&current_album_path).unwrap().into_bytes()).expect("Rabbit hole a") { // captures string in the contents.
        let cover_file_slice = c[0].to_vec(); // turn Capture -> Vec<u8>
        let cover_file = str::from_utf8(&cover_file_slice).expect("Error convert cover str slice!"); // Vec<u8> -> &[u8] -> String
        cover_path =
            Path::new(Box::leak(format!("{}/{}", current_album_path.to_string_lossy(), cover_file).into_boxed_str()));
    }// &ls(&current_album_path).unwrap().into_bytes() will return a reference to the bytestring of current dir file joined with space.

    if cover_path == placeholder {
        to_placeholder(&placeholder, &out_path)?;
    }
    else {
        process(&cover_path, &out_path, &placeholder)?;
    }
    Ok(())
}
