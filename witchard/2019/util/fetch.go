package util

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"strings"
)

func Get(day int) ([]string, error) {
	var in io.ReadCloser
	if _, ok := os.LookupEnv("TEST"); ok {
		// Test mode
		in = os.Stdin
	} else {

		url := fmt.Sprintf("https://adventofcode.com/2019/day/%d/input", day)
		req, err := http.NewRequest("GET", url, nil)
		if err != nil {
			return nil, err
		}

		session := os.Getenv("AOC_SESSION")
		if session == "" {
			return nil, fmt.Errorf("No AOC_SESSION env variable found")
		}

		req.AddCookie(&http.Cookie{Name: "session", Value: session})

		resp, err := (&http.Client{}).Do(req)
		if err != nil {
			return nil, err
		}

		in = resp.Body
	}

	data, err := ioutil.ReadAll(in)
	if err != nil {
		return nil, err
	}

	return strings.Split(string(data), "\n"), nil
}
