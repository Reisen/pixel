import React  from 'react';
import Image  from '../Image';
import styles from './ImageGrid.module.css';

interface Image {
}

interface Props {
    images: Image[];
    width: number;
}

const ImageGrid = (props: Props) => (
    <div className={styles.ImageGrid}>
        {
            props.images.map(image => (
                <Image resolution="800x600" />
            ))
        }

        {
            Array(props.width - (props.images.length % props.width))
                .fill(0)
                .map(_ => <Image resolution="1200x800" />
            )
        }
    </div>
);

export default ImageGrid;
