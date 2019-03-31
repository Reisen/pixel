import React              from 'react';
import Cookies            from 'universal-cookie';
import { Link }           from 'react-router-dom';
import { Image as image } from '../../../../api/types';
import { ScalingMode }    from '../../../../store/images/types';

import Image              from '../Image';
import Pager              from '../Pager';
import styles             from './ImageGrid.module.css';


// HACK, BIG HACK
const findApiBase = () => {
    const cookies = new Cookies();
    return cookies.get('base') || 'http://localhost:3000';
};

interface Props {
    changePage:     (page: number) => void;
    scalingMode:    ScalingMode;
    setScalingMode: (mode: ScalingMode) => void;
    images:         image[];
    width:          number;
    rows:           number;
    page:           number;
}

// Render images that have actually got valid paths.
const renderValidImages = (slice: image[], props: Props) =>
    slice.map((image, k) =>
        <Link key={image.UUID} to={`/i/${image.UUID}`}>
            <Image
                contain={props.scalingMode === 'contain'}
                path={`${findApiBase()}/${image.path}`}
                resolution="800x600"
                width={944 / props.width}
            />
        </Link>
    );


// Render empty spaces for all images that are missing to keep the grid size
// fixed at all times.
const renderEmptyImages = (slice: image[], props: Props) =>
    Array(props.width * props.rows - slice.length)
        .fill(0)
        .map((k, v) =>
            <Image
                empty
                contain={props.scalingMode === 'contain'}
                key={v.toString()}
                width={944 / props.width}
            />
        );


// Render a fixed grid, calculating which images to display depending on page
// width and row count.
const ImageGrid = (props: Props) => {
    const imageView = props.images.slice(
        props.width * props.rows * (props.page - 1 + 0),
        props.width * props.rows * (props.page - 1 + 1)
    );

    if (imageView.length === 0 ) {
        return (
            <div className={styles.OhNo}>
                <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/4/42/Chicken_cartoon_04.svg/723px-Chicken_cartoon_04.svg.png" alt="Chickens" /><br/>
                Nobody here but us chickens!
            </div>
        );
    }

    return (
        <div className={styles.ImageGrid}>
            { renderValidImages(imageView, props) }
            { renderEmptyImages(imageView, props) }

            <Pager
                page={props.page}
                pageCount={5}
                scalingMode={props.scalingMode}
                setPage={props.changePage}
                setScalingMode={props.setScalingMode}
            />
        </div>
    );
}

export default ImageGrid;
