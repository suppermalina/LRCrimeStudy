//
//  ViewController.swift
//  LRCrimeMap
//
//  Created by 马力 on 11/17/18.
//  Copyright © 2018 UALR. All rights reserved.
//

import UIKit
import GoogleMaps

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
        let camera = GMSCameraPosition.camera(withLatitude: 34.736111, longitude:  -92.331111, zoom: 13)
        let mapView = GMSMapView.map(withFrame: CGRect.zero, camera: camera)
        view = mapView
        
        let marker = GMSMarker()
        marker.position = CLLocationCoordinate2DMake(34.736111, -92.331111)
        marker.title = "Little Rock"
        marker.snippet = "AR"
        marker.map = mapView
    }


}

